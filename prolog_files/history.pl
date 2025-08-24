% ================================================
% Knowledge Base: Bulgarian Historical Sites
% ================================================

:- module(history, [
    historical_site/4,
    located_in/2,
    built_in_period/2,
    site_type/2,
    sites_by_period/2,
    sites_by_type/2,
    sites_in_location/2,
    site_details/2,
    unload_history/0
]).

% --------------------------------
% Исторически забележителности в България
% --------------------------------

% Форма: historical_site(Name, Period, Location, Type).
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

% --------------------------------
% Допълнителна информация
% --------------------------------
located_in(Name, Location) :- historical_site(Name, _, Location, _).
built_in_period(Name, Period) :- historical_site(Name, Period, _, _).
site_type(Name, Type) :- historical_site(Name, _, _, Type).

% --------------------------------
% Списъци по критерии
% --------------------------------
sites_by_period(Period, Name) :-
    historical_site(Name, Period, _, _).

sites_by_type(Type, Name) :-
    historical_site(Name, _, _, Type).

sites_in_location(Location, Name) :-
    historical_site(Name, _, Location, _).

% --------------------------------
% Съставен предикат: пълни детайли
% --------------------------------
site_details(Name, Details) :-
    historical_site(Name, Period, Location, Type),
    format(string(Details),
        "=== ~w ===~nLocation: ~w~nPeriod: ~w~nType: ~w~n",
        [Name, Location, Period, Type]).

% --------------------------------
% Възможност за "разтоварване" от паметта
% (ако сменяме база знания динамично)
% --------------------------------
unload_history :-
    abolish(historical_site/4),
    abolish(located_in/2),
    abolish(built_in_period/2),
    abolish(site_type/2),
    abolish(sites_by_period/2),
    abolish(sites_by_type/2),
    abolish(sites_in_location/2),
    abolish(site_details/2).
