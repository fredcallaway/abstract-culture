
# env = RedBlackEnv(N=100, S=10, D=200, p_r=1)
# sim = simulate(env, 20; init=.01)

# mats = map(sim) do pop
#     red = zeros(env.S, env.S)
#     black = zeros(env.S, env.S)
#     for beh in pop
#         if beh.red
#             red[beh.a, beh.b] += 1
#         else
#             black[beh.a, beh.b] += 1
#         end
#     end
#     (;red, black)
# end

# %% --------

N1 = 80
N2 = 160

df = dataframe(grid(N = [N1, N2], S=5, D=-1, p_r=1.)) do prm
    flatmap(1:3) do rep
        env = RedBlackEnv(;prm...)
        sim = simulate(env, 9; init=1/env.N)
        flatmap(enumerate(sim)) do (gen, pop)
            map(pop[:]) do beh
                (;gen, rep, beh.a, beh.b, beh.red)
            end
        end
    end
end


R"""
do_plot = function(df) {
    lim = max(df$a)
    df %>%
        filter(a > 0, b > 0) %>%
        group_by(N, gen, rep, a, b) %>%
        summarise(red_rate = mean(red), n = n()) %>%
        rename(pop = rep) %>%
        ggplot(aes(a, b, color=red_rate, size=n)) +
        geom_point(shape="square") + scale_size_area(max_size=3) +
        no_gridlines +
        coord_fixed(expand=F) +
        geom_hline(yintercept=seq(.5, lim + .5), linetype="solid", color="gray90", linewidth=.4) +
        geom_vline(xintercept=seq(.5, lim + .5), linetype="solid", color="gray90", linewidth=.4) +
        no_axes +
        scale_color_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=85, p1=1, p2=2, mid=0.5, rev=F, lim=c(0, 1)) +
        facet_grid(pop~gen)
}



($df %>% filter(N==$N1) %>% do_plot + ggtitle(paste0("N = ", $N1))) /
($df %>% filter(N==$N2) %>% do_plot + ggtitle(paste0("N = ", $N2)))

fig(w=14, h=10)
"""

# %% --------

df = dataframe(grid(N = 100, S=5, D=5, p_r=1.)) do prm
    flatmap(1:3) do rep
        env = RedBlackEnv(;prm...)
        (;S, N, K) = env


        X = falses(S, S)
        # for i in sample(1:S^2, 1; replace=false)
        #     X[i] = true
        # end
        X[1] = true

        pop1 = map(Iterators.product(1:S, 1:S)) do (a, b)
            Behavior(a, b, X[a, b])
        end
        pop1 = reshape(repeat(pop1, N ÷ length(pop1)), K, N)

        sim = simulate(env, 9; init=pop1)
        flatmap(enumerate(sim)) do (gen, pop)
            map(pop[:]) do beh
                (;gen, rep, beh.a, beh.b, beh.red)
            end
        end
    end
end

R"""
$df %>% do_plot
fig(w=14, h=5)
"""

# %% --------

N =

df = dataframe(grid(;N, S=5, D=50, p_r=1., focused=[true, false])) do prm
    flatmap(1:3) do rep
        env = RedBlackEnv(;delete(prm, :focused)...)
        (;S, N, K) = env

        if prm.focused
            pop1 = map(Iterators.product(1:S, 1:S)) do (a, b)
                Behavior(a, b, a == b == 1)
            end
            init = reshape(repeat(pop1, N ÷ length(pop1)), K, N)
        else
            init = 1/S^2
        end

        sim = simulate(env, 29; init)
        flatmap(enumerate(sim)) do (gen, pop)
            map(pop[:]) do beh
                (;gen, rep, beh.a, beh.b, beh.red)
            end
        end
    end
end

R"""
data = $df %>%
    rowwise() %>%
    filter((gen <= 5) || (mod(gen, 5) == 0))

(data %>% filter(focused) %>% do_plot + ggtitle(paste0("N = ", $N))) /
(data %>% filter(!focused) %>% do_plot + ggtitle(paste0("N = ", $N)))

fig(w=14, h=10)
"""

# %% --------

N = 200
df = dataframe(grid(;N, S=5, D=50, p_r=1., focused=false)) do prm
    flatmap(1:3) do rep
        env = RedBlackEnv(;delete(prm, :focused)...)
        (;S, N, K) = env

        if prm.focused
            pop1 = map(Iterators.product(1:S, 1:S)) do (a, b)
                Behavior(a, b, a == b == 1)
            end
            init = reshape(repeat(pop1, N ÷ length(pop1)), K, N)
        else
            init = 1/S^2
        end

        sim = simulate(env, 99; init)
        flatmap(enumerate(sim)) do (gen, pop)
            map(pop[:]) do beh
                (;gen, rep, beh.a, beh.b, beh.red)
            end
        end
    end
end

R"""
foo = function(df) {
    lim = max(df$a)
    df %>%
        filter(rep == 1) %>%
        filter(gen == 30) %>%
        filter(a > 0, b > 0) %>%
        group_by(N, gen, rep, a, b) %>%
        summarise(red_rate = mean(red), n = n()) %>%
        rename(pop = rep) %>%
        ggplot(aes(a, b, color=red_rate, size=n, group=interaction(a, b))) +
        geom_point(shape="square") + scale_size_area(max_size=10) +
        no_gridlines +
        coord_fixed(expand=F) +
        geom_hline(yintercept=seq(.5, lim + .5), linetype="solid", color="gray90", linewidth=.4) +
        geom_vline(xintercept=seq(.5, lim + .5), linetype="solid", color="gray90", linewidth=.4) +
        no_axes +
        scale_color_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=85, p1=1, p2=2, mid=0.5, rev=F, lim=c(0, 1)) +
        facet_grid(~pop)
}

foo($df)
fig()
"""

# %% --------


R"""
library(gganimate)
do_anim = function(df) {
    lim = max(df$a)
    df %>%
        filter(rep == 1) %>%
        filter(a > 0, b > 0) %>%
        group_by(N, gen, rep, a, b) %>%
        summarise(red_rate = mean(red), n = n()) %>%
        rename(pop = rep) %>%
        ggplot(aes(a, b, color=red_rate, size=n, group=interaction(a, b))) +
        geom_point(shape="square") + scale_size_area(max_size=8) +
        no_gridlines +
        coord_fixed(expand=F) +
        geom_hline(yintercept=seq(.5, lim + .5), linetype="solid", color="gray90", linewidth=.4) +
        geom_vline(xintercept=seq(.5, lim + .5), linetype="solid", color="gray90", linewidth=.4) +
        no_axes +
        scale_color_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=85, p1=1, p2=2, mid=0.5, rev=F, lim=c(0, 1)) +
        facet_grid(~pop) +
        transition_manual(gen)
        # transition_time(gen) +
        # ease_aes('cubic-in-out') +
        # labs(title = 'Generation {frame_time}')
}
do_anim($df) %>% animate(nframes=100, fps=5)
anim_save("evolution.gif")
"""











# %% ==================== old ====================


R"""
$df %>%
    group_by(gen, a, b) %>%
    summarise(red_rate = mean(red), N = n()) %>%
    ggplot(aes(a, b, fill=red_rate)) +
    geom_tile() +
    facet_wrap(~gen, nrow=2) +
    no_gridlines +
    no_axes +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=85, p1=1, p2=2, mid=0.5, rev=F)

fig(w=8, h=3)
"""

# %% --------

g = grid(red_rate=0:.1:1, N=1:11)[:] |> DataFrame

R"""
$g %>% ggplot(aes(factor(red_rate), factor(N), color=red_rate, size=N)) +
    geom_point(shape="circle") +
    scale_color_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=85, p1=1, p2=2, mid=0.5, rev=F) +
    # coord_cartesian(expand=F) +
    geom_hline(yintercept=seq(.5, 5.5), linetype="solid", color="gray90") +
    geom_vline(xintercept=seq(.5, 5.5), linetype="solid", color="gray90") +
    # no_axes +
    theme() +
    scale_size_area(max_size=4)

fig()
"""

R"""
$g %>%
    ggplot(aes(red_rate, N, fill=red_rate, alpha=N)) +
    geom_tile() +
    no_gridlines +
    no_axes +
    scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=50, p1=2, p2=5, mid=0.5, rev=F)

fig(w=8, h=3)
"""

R"""
$g %>%
    mutate(rrc = red_rate - 0.5) %>%
    ggplot(aes(red_rate, N, fill = hcl(
      h = scales::rescale(sign(rrc) * abs(rrc)^.3, to = c(197, 360)),
      l = scales::rescale(abs(rrc), to = c(80, 20)),
      c = scales::rescale(N, to = c(10, 100))
    ))) +
    geom_tile() +
    no_gridlines +
    scale_fill_identity() +
    no_axes +
    theme()
    # scale_fill_continuous_diverging(h1=197, h2=350, c1=180, l1=20, l2=380, p1=1, p2=2, mid=0.5, rev=F)

fig(w=8, h=3)
"""



# %% --------

R"""
$df %>%
    # summarise(red_rate = mean(red), N = n()) %>%
    # ggplot(aes(factor(a), factor(b), color=factor(a))) +
    ggplot(aes(a, b, color=factor(red))) +
    geom_jitter(width=.3, height=.3, size=.2) +
    facet_wrap(~gen, nrow=2) +
    geom_hline(yintercept=seq(.5, 5.5), linetype="solid", color="gray90") +
    geom_vline(xintercept=seq(.5, 5.5), linetype="solid", color="gray90") +
    no_gridlines +
    coord_fixed(expand=F) +
    # no_axes +
    ggtitle("N = 100")


fig(w=8, h=3)
"""
