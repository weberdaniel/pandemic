{application, pandemic, 
[{vsn,"1.0.0"},
 {modules, [pandemic, world, town, town_sup,
            sup_world, auth, calc, player, player_sup, sup_auth,
            vehicle, xml_parser, pandemic_sup]},
 {registered, []},
 {mod, {pandemic, []}},
 {description, "A nice erlang game"},
 {applications, [stdlib, kernel]},
 {env, [{gameconf,"config/game_config.xml"},
        {authconf,"config/authfile"}
       ]}
]}.
