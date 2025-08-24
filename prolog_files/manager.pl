% Зареждаме history
load_kb('history.pl').

% Използваме я
sites_by_type(fortress, Name).

% После я махаме
unload_history.
