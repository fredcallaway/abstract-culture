using StatsPlots
#using StatsPlots.Measures
using Dates
mkpath(".fighist")
mkpath("figs")
gr(label="", dpi=200, size=(400,300), lw=2, widen=true)
ENV["GKSwstype"] = "nul"

function figure(f, name="tmp"; base="figs", pdf=false, kws...)
    plot(;kws...)
    f()
    dt = Dates.format(now(), "m-d-H-M-S")
    flatname = replace(name, "/" => "-")
    p = ".fighist/$dt-$flatname.png"
    savefig(p)
    if name != "tmp"
        if pdf
            println("$base/$name.pdf")
            savefig("$base/$name.pdf")
        else
            cp(p, "$base/$name.png"; force=true)
        end
    end
end
