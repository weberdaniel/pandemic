{application, pandemic, 
[{vsn,"1.0.0"},
 {modules, [pandemic, world, country, town, town_sup, country_sup,
            world_sup]},
 {registered, []},
 {mod, {pandemic, []}},
 {description, "A nice erlang game"},
 {applications, [stdlib, kernel]},
 {env, [{filename,"config/game_config.xml"}]}
]}.