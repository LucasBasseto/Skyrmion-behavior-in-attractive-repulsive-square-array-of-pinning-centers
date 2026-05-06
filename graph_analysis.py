import matplotlib.pyplot as plt
from matplotlib import rcParams, cycler
import pandas as pd
import numpy as np
import os

# As importações abaixo não foram usadas no seu código, mas as mantive caso você as use em outro lugar.
# from scipy import signal
# from sklearn.cluster import DBSCAN

def FixPlot(lx: float, ly: float):
    rcParams['font.family'] = 'serif'
    
    # removido o uso de LaTeX
    # rcParams['text.latex.preamble'] = "\\usepackage{stix}"
    # rcParams['text.usetex'] = True
    rcParams['font.size'] = 28
    rcParams['axes.linewidth'] = 1.1
    rcParams['axes.labelpad'] = 10.0
    plot_color_cycle = cycler('color', ['000000', 'FE0000', '0000FE', '008001', 'FD8000', '8c564b',
                                         'e377c2', '7f7f7f', 'bcbd22', '17becf'])
    rcParams['axes.prop_cycle'] = plot_color_cycle
    rcParams['axes.xmargin'] = 0
    rcParams['axes.ymargin'] = 0
    rcParams['legend.fancybox'] = False
    rcParams['legend.framealpha'] = 1.0
    rcParams['legend.edgecolor'] = "black"
    rcParams['legend.fontsize'] = 22
    rcParams['xtick.labelsize'] = 22
    rcParams['ytick.labelsize'] = 22

    rcParams['ytick.right'] = True
    rcParams['xtick.top'] = True

    rcParams['xtick.direction'] = "in"
    rcParams['ytick.direction'] = "in"
    rcParams['axes.formatter.useoffset'] = False

    rcParams.update({"figure.figsize": (lx, ly),
                     "figure.subplot.left": 0.177, "figure.subplot.right": 0.946,
                     "figure.subplot.bottom": 0.156, "figure.subplot.top": 0.965,
                     #"axes.autolimit_mode": "round_numbers",
                     "xtick.major.size": 7,
                     "xtick.minor.size": 3.5,
                     "xtick.major.width": 1.1,
                     "xtick.minor.width": 1.1,
                     "xtick.major.pad": 5,
                     "xtick.minor.visible": True,
                     "ytick.major.size": 7,
                     "ytick.minor.size": 3.5,
                     "ytick.major.width": 1.1,
                     "ytick.minor.width": 1.1,
                     "ytick.major.pad": 5,
                     "ytick.minor.visible": True,
                     "lines.markersize": 10,
                     "lines.markeredgewidth": 0.8,
                     "mathtext.fontset": "cm"})


def point_from_name(name: str) -> list[float]:
    if name.endswith(".dat"):
        name = name[:-4] 
    name_split = name.split("_") 
    F_REP = float(name_split[-1])
    F_ATRAT = float(name_split[-4])
    beta = float(name_split[-7])
    return [beta, F_ATRAT, F_REP]

def name_from_point(beta: float, f_atrat: float, f_rep: float) -> str:
    return f"velocidade_global_beta_{beta:.4f}_F_ATRAT_{f_atrat:.4f}_F_REP_{f_rep:.4f}.dat"

# Assume que os arquivos .dat estão no mesmo diretório do script
files = [i.name for i in os.scandir("./") if i.name.endswith(".dat")]

ANGLE_THRESHOLD = 1 

def deltaFd_vs_Fat(fixed_f_rep: float):
    points = np.array(sorted([point_from_name(i) for i in files]))
    valid = np.abs(points[:, 2] - fixed_f_rep) < 0.005
    points = points[valid, :]
    FixPlot(20, 10)
    fig, ax = plt.subplots(nrows=3)
    pthetas = [-60, -45, 0]
    deltaFd = [[] for _ in pthetas]
    for point in points:
        data = pd.read_fwf(name_from_point(*point), header=None, skiprows=1)

        thetas = data[3].to_numpy()
        Fd = data[0].to_numpy()
        V = np.sqrt(data[1].to_numpy() ** 2 + data[2].to_numpy() ** 2)

        for idx, t in enumerate(pthetas):
            valid_angle_range = np.abs(thetas - t) < ANGLE_THRESHOLD
            Fdt = Fd[valid_angle_range]
            if Fdt.shape[0] > 0:
                deltaFd[idx].append(np.ptp(Fdt))
            else:
                deltaFd[idx].append(np.nan)
        
        ax[-2].plot(Fd, thetas, lw=2, zorder=1) 
        ax[0].plot(Fd, V, lw=2) 
    
    ax[-2].set_xlabel("$F_\\mathrm{D}$")
    ax[-2].set_ylabel("$\\theta$")
    ax[-2].set_xlim((0, 3))
    ax[0].set_xlabel("$F_\\mathrm{D}$")
    ax[0].set_ylabel("$\\langle V\\rangle$")
    ax[0].set_xlim((0, 3))
    ax[0].set_ylim(0, 2)

    for idx, Fds in enumerate(deltaFd):
        if not np.isnan(Fds).all():
            ax[-1].plot(points[:, 1], Fds, "-o", label=f"$\\theta={pthetas[idx]:.0f}^\\circ$")
    ax[-1].legend()
    ax[-1].set_xlabel("$F_\\mathrm{at}$")
    ax[-1].set_ylabel("$\\Delta F_\\mathrm{D}$")
    ax[-1].text(0.5, 0.5, f"$F_\\mathrm{{rep}}={fixed_f_rep:.1f}$", ha="center", va="center", transform=ax[-1].transAxes)
    fig.subplots_adjust(hspace=0.5)
    fig.savefig(f"./fig1_Frep_{fixed_f_rep:.1f}_AT{ANGLE_THRESHOLD:.1f}_ALLDATA.png", dpi=275, facecolor="white", bbox_inches="tight")
    plt.close(fig) 

def validate_deltaFd_vs_Fat(fixed_f_rep: float):
    points = np.array(sorted([point_from_name(i) for i in files]))
    valid = np.abs(points[:, 2] - fixed_f_rep) < 0.005
    points = points[valid, :]
    
    FixPlot(24, 40) 
    fig, ax = plt.subplots(nrows=len(points), ncols=2, figsize=(24, len(points) * 5)) 
    
    pthetas = [-60, -45, 0]
    colors = ["red", "blue", "green"]
    
    for idx, point in enumerate(points):
        data = pd.read_fwf(name_from_point(*point), header=None, skiprows=1)

        V = np.sqrt(data[1].to_numpy() ** 2 + data[2].to_numpy() ** 2)
        Fd = data[0].to_numpy()
        thetas = data[3].to_numpy() 

        ax[idx][0].plot(Fd, V, lw=2, color="black")
        ax[idx][0].set_xlabel("$F_D$")
        ax[idx][0].set_ylabel("$\\langle V\\rangle$")
        ax[idx][0].set_xlim(-0.1, 3.1)
        ax[idx][0].set_ylim(0, max(1.0, V.max() * 1.1)) 

        ax[idx][1].plot(Fd, thetas, lw=1, color="black")
        ax[idx][1].set_xlabel("$F_D$")
        ax[idx][1].set_ylabel("$\\theta$")
        ax[idx][1].set_xlim(-0.1, 3.1)
        ax[idx][1].set_ylim(-90, 90) 
        ax[idx][1].set_yticks(pthetas) 
        
        thetas_for_fd_range = thetas 
        Fd_for_fd_range = Fd

        for jdx, t in enumerate(pthetas):
            valid_angle_range = np.abs(thetas_for_fd_range - t) < ANGLE_THRESHOLD
            Fdt = Fd_for_fd_range[valid_angle_range]
            
            ax[idx][1].axhline(t - ANGLE_THRESHOLD, color=colors[jdx], linestyle='--', lw=1, zorder=0)
            ax[idx][1].axhline(t + ANGLE_THRESHOLD, color=colors[jdx], linestyle='--', lw=1, zorder=0)
            ax[idx][1].axhline(t, color=colors[jdx], linestyle='-', lw=2, zorder=0)


            if Fdt.shape[0] > 0:
                ax[idx][1].axvline(Fdt.max(), lw=2, color=colors[jdx])
                ax[idx][1].axvline(Fdt.min(), lw=2, color=colors[jdx])
                ax[idx][0].axvline(Fdt.max(), lw=2, color=colors[jdx])
                ax[idx][0].axvline(Fdt.min(), lw=2, color=colors[jdx])
        
        handles = [plt.Line2D([0], [0], color=colors[jdx], lw=2, label=f"$\\theta={t:.0f}^\\circ$") for jdx, t in enumerate(pthetas)]
        ax[idx][1].legend(handles=handles, loc='upper right', fontsize=16)


        if (idx + 1) != len(points):
            ax[idx][0].set_xticklabels([])
            ax[idx][1].set_xticklabels([])
        else:
            ax[idx][0].set_xlabel("$F_D$")
            ax[idx][1].set_xlabel("$F_D$")

        ax[idx][0].annotate(f"$F_\\mathrm{{at}}={point[1]:+.1f}$", (0.95, 0.05), xycoords=ax[idx][0].transAxes, ha="right", va="bottom")
        
    fig.subplots_adjust(hspace=0.05)
    fig.savefig(f"./validate_fig1_Frep_{fixed_f_rep:.1f}_AT{ANGLE_THRESHOLD:.1f}_NO_HIST.png", dpi=275, facecolor="white", bbox_inches="tight")
    plt.close(fig)

def deltaFd_vs_Fep(fixed_f_atrat: float):
    points = np.array(sorted([point_from_name(i) for i in files]))
    valid = np.abs(points[:, 1] - fixed_f_atrat) < 0.005
    points = points[valid, :]
    FixPlot(20, 10)
    fig, ax = plt.subplots(nrows=3)
    pthetas = [-60, -45, 0]
    deltaFd = [[] for _ in pthetas]
    for point in points:
        data = pd.read_fwf(name_from_point(*point), header=None, skiprows=1)

        thetas = data[3].to_numpy()
        Fd = data[0].to_numpy()
        V = np.sqrt(data[1].to_numpy() ** 2 + data[2].to_numpy() ** 2)

        for idx, t in enumerate(pthetas):
            valid_angle_range = np.abs(thetas - t) < ANGLE_THRESHOLD
            Fdt = Fd[valid_angle_range]
            if Fdt.shape[0] > 0:
                deltaFd[idx].append(np.ptp(Fdt))
            else:
                deltaFd[idx].append(np.nan)
        
        ax[-2].plot(Fd, thetas, lw=2, zorder=1) 
        ax[0].plot(Fd, V, lw=2) 
    
    ax[-2].set_xlabel("$F_\\mathrm{D}$")
    ax[-2].set_ylabel("$\\theta$")
    ax[-2].set_xlim((0, 3))
    ax[0].set_xlabel("$F_\\mathrm{D}$")
    ax[0].set_ylabel("$\\langle V\\rangle$")
    ax[0].set_xlim((0, 3))
    ax[0].set_ylim(0, 2)

    for idx, Fds in enumerate(deltaFd):
        if not np.isnan(Fds).all():
            ax[-1].plot(points[:, 2], Fds, "-o", label=f"$\\theta={pthetas[idx]:.0f}^\\circ$")
    ax[-1].legend()
    ax[-1].set_xlabel("$F_\\mathrm{rep}$")
    ax[-1].set_ylabel("$\\Delta F_\\mathrm{D}$")
    ax[-1].text(0.5, 0.5, f"$F_\\mathrm{{at}}={fixed_f_atrat:+.1f}$", ha="center", va="center", transform=ax[-1].transAxes)
    fig.subplots_adjust(hspace=0.5)
    fig.savefig(f"./fig2_Fatrat_{fixed_f_atrat:+.1f}_AT{ANGLE_THRESHOLD:.1f}_ALLDATA.png", dpi=275, facecolor="white", bbox_inches="tight")
    plt.close(fig) 

def validate_deltaFd_vs_Frep(fixed_f_atrat: float):
    points = np.array(sorted([point_from_name(i) for i in files]))
    valid = np.abs(points[:, 1] - fixed_f_atrat) < 0.005
    points = points[valid, :]
    
    FixPlot(24, 40) 
    fig, ax = plt.subplots(nrows=len(points), ncols=2, figsize=(24, len(points) * 5)) 
    
    pthetas = [-60, -45, 0]
    colors = ["red", "blue", "green"]
    for idx, point in enumerate(points):
        data = pd.read_fwf(name_from_point(*point), header=None, skiprows=1)

        V = np.sqrt(data[1].to_numpy() ** 2 + data[2].to_numpy() ** 2)
        Fd = data[0].to_numpy()
        thetas = data[3].to_numpy()

        ax[idx][0].plot(Fd, V, lw=2, color="black") 
        ax[idx][0].set_xlabel("$F_D$")
        ax[idx][0].set_ylabel("$\\langle V\\rangle$")
        ax[idx][0].set_xlim(-0.1, 3.1)
        ax[idx][0].set_ylim(0, max(1.0, V.max() * 1.1)) 
        
        ax[idx][1].plot(Fd, thetas, lw=1, color="black") 
        ax[idx][1].set_xlabel("$F_D$")
        ax[idx][1].set_ylabel("$\\theta$")
        ax[idx][1].set_xlim(-0.1, 3.1)
        ax[idx][1].set_ylim(-90, 90) 
        ax[idx][1].set_yticks(pthetas) 
        
        thetas_for_fd_range = thetas 
        Fd_for_fd_range = Fd

        for jdx, t in enumerate(pthetas):
            valid_angle_range = np.abs(thetas_for_fd_range - t) < ANGLE_THRESHOLD
            Fdt = Fd_for_fd_range[valid_angle_range]
            
            ax[idx][1].axhline(t - ANGLE_THRESHOLD, color=colors[jdx], linestyle='--', lw=1, zorder=0)
            ax[idx][1].axhline(t + ANGLE_THRESHOLD, color=colors[jdx], linestyle='--', lw=1, zorder=0)
            ax[idx][1].axhline(t, color=colors[jdx], linestyle='-', lw=2, zorder=0)


            if Fdt.shape[0] > 0:
                ax[idx][1].axvline(Fdt.max(), lw=2, color=colors[jdx]) 
                ax[idx][1].axvline(Fdt.min(), lw=2, color=colors[jdx]) 
                ax[idx][0].axvline(Fdt.max(), lw=2, color=colors[jdx]) 
                ax[idx][0].axvline(Fdt.min(), lw=2, color=colors[jdx]) 
        
        handles = [plt.Line2D([0], [0], color=colors[jdx], lw=2, label=f"$\\theta={t:.0f}^\\circ$") for jdx, t in enumerate(pthetas)]
        ax[idx][1].legend(handles=handles, loc='upper right', fontsize=16)

        if (idx + 1) != len(points):
            ax[idx][0].set_xticklabels([])
            ax[idx][1].set_xticklabels([])
        else:
            ax[idx][0].set_xlabel("$F_D$")
            ax[idx][1].set_xlabel("$F_D$")

        ax[idx][0].annotate(f"$F_\\mathrm{{rep}}={point[2]:+.1f}$", (0.95, 0.05), xycoords=ax[idx][0].transAxes, ha="right", va="bottom")
        
    fig.subplots_adjust(hspace=0.05)
    fig.savefig(f"./validate_fig2_Fatrat_{fixed_f_atrat:+.1f}_AT{ANGLE_THRESHOLD:.1f}_NO_HIST.png", dpi=275, facecolor="white", bbox_inches="tight")
    plt.close(fig)


def get_unique_params(files: list[str]):
    all_points = np.array([point_from_name(i) for i in files])
    unique_betas = np.unique(all_points[:, 0])
    unique_fatrats = np.unique(all_points[:, 1])
    unique_freps = np.unique(all_points[:, 2])
    return unique_betas, unique_fatrats, unique_freps

# --- NOVA FUNÇÃO PARA EXPORTAR OS DADOS ---
def export_deltaFd_data(files: list[str], angle_threshold: float):
    print("\n--- Coletando dados para exportação de DeltaFd x Fat x Frep por ângulo ---")
    
    pthetas = [-60, -45, 0]
    
    # Dicionário para armazenar os dados de cada ângulo
    # key: angulo, value: lista de [Fat, Frep, DeltaFd]
    deltaFd_data_by_angle = {angle: [] for angle in pthetas}

    all_points = np.array(sorted([point_from_name(i) for i in files]))

    for point in all_points:
        beta, fatrat, frep = point
        file_name = name_from_point(beta, fatrat, frep)
        
        try:
            data = pd.read_fwf(file_name, header=None, skiprows=1)
            thetas = data[3].to_numpy()
            Fd = data[0].to_numpy()

            for t_target in pthetas:
                valid_angle_range = np.abs(thetas - t_target) < angle_threshold
                Fdt = Fd[valid_angle_range]
                
                if Fdt.shape[0] > 0:
                    delta_fd_value = np.ptp(Fdt)
                    deltaFd_data_by_angle[t_target].append([fatrat, frep, delta_fd_value])
                # else:
                    # Se não houver dados para o ângulo, não adicionamos. np.nan pode ser adicionado se necessário.
                    # deltaFd_data_by_angle[t_target].append([fatrat, frep, np.nan])
        except FileNotFoundError:
            print(f"Aviso: Arquivo {file_name} não encontrado. Pulando.")
        except Exception as e:
            print(f"Erro ao processar {file_name}: {e}")

    print("\n--- Exportando dados ---")
    for angle, data_list in deltaFd_data_by_angle.items():
        output_filename = f"DeltaFd_Fat_Frep_Angle_{angle:.0f}deg.dat"
        with open(output_filename, "w") as f:
            f.write(f"# Dados de Delta Fd (F_Dmax - F_Dmin) para angulo {angle:.0f} graus\n")
            f.write(f"# Tolerancia angular (ANGLE_THRESHOLD): +/- {angle_threshold} graus\n")
            f.write(f"#\n")
            f.write(f"# {'F_atrat':<15} {'F_rep':<15} {'Delta_Fd':<15}\n")
            f.write(f"# {'-'*15} {'-'*15} {'-'*15}\n")
            
            # Ordenar os dados para melhor análise, primeiro por Fatrat, depois por Frep
            sorted_data = sorted(data_list, key=lambda x: (x[0], x[1]))
            
            for row in sorted_data:
                f.write(f"  {row[0]:<15.4f} {row[1]:<15.4f} {row[2]:<15.4f}\n")
        print(f"Arquivo '{output_filename}' gerado com sucesso.")

# --- Execução principal do script ---

# Obter os valores únicos de parâmetros
unique_betas, unique_fatrats, unique_freps = get_unique_params(files)

print(f"Valores únicos de Beta: {unique_betas}")
print(f"Valores únicos de F_ATRAT: {unique_fatrats}")
print(f"Valores únicos de F_REP: {unique_freps}")

# Loop para deltaFd_vs_Fat e validate_deltaFd_vs_Fat (fixando F_REP)
for f_rep_val in unique_freps:
    print(f"\nGerando gráficos para F_REP = {f_rep_val:.1f} (AT={ANGLE_THRESHOLD}, NO HIST)...")
    deltaFd_vs_Fat(f_rep_val)
    validate_deltaFd_vs_Fat(f_rep_val)

# Loop para deltaFd_vs_Fep e validate_deltaFd_vs_Frep (fixando F_ATRAT)
for f_atrat_val in unique_fatrats:
    print(f"\nGerando gráficos para F_ATRAT = {f_atrat_val:+.1f} (AT={ANGLE_THRESHOLD}, NO HIST)...") 
    deltaFd_vs_Fep(f_atrat_val)
    validate_deltaFd_vs_Frep(f_atrat_val)

# --- Chamada para a nova função de exportação de dados ---
export_deltaFd_data(files, ANGLE_THRESHOLD)

print("\nGeração de todos os gráficos e arquivos de dados concluída!")
