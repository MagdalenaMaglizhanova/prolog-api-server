% Исторически забележителности в България
spring(1, 'Belchin-Verila', 46, 354, 65.36, 0, 2.5, [6, 13.83, 122.22, 36.01, 6.11, 5.35, 0, 0], [0.1, 0.06, 94.25, 0.9, 3.61, 0, 0.03, 0.0002]):-write('eho').
% Форма: historical_site(Name, period, location, type).
historical_site("Thracian Tomb of Kazanlak", thracian, "Kazanlak", tomb).
historical_site("Roman Theatre of Plovdiv", roman, "Plovdiv", theatre).
historical_site("Boyana Church", medieval, "Sofia", church).
historical_site("Tsarevets Fortress", medieval, "Veliko Tarnovo", fortress).
historical_site("Baba Vida Fortress", medieval, "Vidin", fortress).
historical_site("Perperikon", thracian, "Eastern Rhodopes", archaeological_site).
historical_site("Rila Monastery", medieval, "Rila Mountains", monastery).
historical_site("Asen Fortress", medieval, "Asenovgrad", fortress).
historical_site("Shumen Fortress", medieval, "Shumen", fortress).
historical_site("Pliska", early_medieval, "Pliska", capital_city).

% Допълнителна информация
located_in(Name, Location) :- historical_site(Name, _, Location, _).
built_in_period(Name, Period) :- historical_site(Name, Period, _, _).
site_type(Name, Type) :- historical_site(Name, _, _, Type).

% Списък на всички забележителности от даден период
sites_by_period(Period, Name) :- historical_site(Name, Period, _, _).

% Списък на всички забележителности от даден тип
sites_by_type(Type, Name) :- historical_site(Name, _, _, Type).

% Списък на всички забележителности в даден град/регион
sites_in_location(Location, Name) :- historical_site(Name, _, Location, _).

% Съставен предикат: покажи пълни детайли
site_details(Name, Details) :-
    historical_site(Name, Period, Location, Type),
    format(string(Details),
        "=== ~w ===~nLocation: ~w~nPeriod: ~w~nType: ~w~n",
        [Name, Location, Period, Type]).
