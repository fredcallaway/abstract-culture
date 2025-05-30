using CairoMakie
using Dates
mkpath(".fighist")
mkpath("figs")
# gr(label="", dpi=200, size=(400,300), lw=2)
# ENV["GKSwstype"] = "nul"

if !@isdefined(FIGS_PATH)
    FIGS_PATH = "figs/"
end
function figure(func, name="tmp", size=(600,300); figs_path=FIGS_PATH, resolution=3, pdf=false, kws...)
    f = Figure(;size, kws...);
    Axis(f[1, 1]);
    func()
    dt = Dates.format(now(), "mmdd-HHMMSSss")
    path = ".fighist/$dt-$(replace(name, "/" => "-")).png"
    save(path, current_figure(), px_per_unit=resolution)
    if name != "tmp"
        mkpath(dirname("$figs_path-$name"))
        if pdf
            save("$figs_path$name.pdf", current_figure())
        else
            print("$figs_path$name.png")
            cp(path, "$figs_path$name.png"; force=true)
        end
    end
end

function facet_grid(f, name::String, ncol::Int, nrow::Int=1; size=300, kws...)
    figure(name) do
        fig = Figure(size=400 .* (ncol, nrow); kws...)
        for row=1:nrow, col=1:ncol
            ax = Axis(fig[row, col])
            f(col, row)
        end
    end
end
facet_grid(f, ncol::Int, nrow::Int=1; kws...) = facet_grid(f, "tmp", ncol, nrow; kws...)