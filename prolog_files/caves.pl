% ======================================
% Caves in Bulgaria
% cave(Name, MountainRange, Region, Location, Type, Length_m, Depth_m).
% ======================================

cave("Magura Cave", "Western Balkan Mountains", "Northwest Bulgaria", "Rabisha, Belogradchik", "karst, cave paintings", 2500, 0).
cave("Ledenika Cave", "Vratsa Balkan", "Northwest Bulgaria", "Vratsa", "karst, show cave", 320, 60).
cave("Devil's Throat", "Rhodope Mountains", "South Bulgaria", "Trigrad", "karst, abyss cave, underground waterfall", 1750, 150).
cave("Yagodina Cave", "Rhodope Mountains", "South Bulgaria", "Yagodina", "karst, show cave", 10500, 36).
cave("Saeva Dupka", "Central Balkan Mountains", "North Central Bulgaria", "Brestnitsa, Lovech", "karst, show cave", 400, 25).
cave("Prohodna Cave", "Iskar Gorge, Balkan Mountains", "Northwest Bulgaria", "Karlukovo, Lukovit", "karst, natural rock bridge, 'Eyes of God'", 262, 45).
cave("Bacho Kiro Cave", "Central Balkan Mountains", "North Central Bulgaria", "near Dryanovo Monastery", "karst, archaeological site", 3600, 0).
cave("Uhlovitsa Cave", "Rhodope Mountains", "South Bulgaria", "Mogilitsa, Smolyan", "karst, show cave", 460, 30).
cave("Orlova Chuka Cave", "Rusenski Lom Plateau", "North Bulgaria", "Pepelina, Ruse", "karst, archaeological finds", 13500, 20).
cave("Devetashka Cave", "Fore-Balkan", "North Central Bulgaria", "near Lovech", "karst, huge entrance hall, bat colonies", 2440, 60).

% Additional caves by mountain ranges

% Rhodope Mountains
cave("Snezhanka Cave", "Rhodope Mountains", "South Bulgaria", "Peshtera", "karst, show cave", 145, 0).
cave("Haramiiska Cave", "Rhodope Mountains", "South Bulgaria", "Trigrad Gorge", "karst, vertical cave", 510, 150).

% Pirin Mountains
cave("Banderitsa Cave", "Pirin Mountains", "Southwest Bulgaria", "near Bansko", "karst, deep cave", 1500, 125).
cave("Yavorova Cave", "Pirin Mountains", "Southwest Bulgaria", "near Yavorov hut", "karst, deep cave", 2750, 150).

% Strandzha Mountains
cave("Mishkova Dupka", "Strandzha Mountains", "Southeast Bulgaria", "Malko Tarnovo", "karst, archaeological finds", 125, 30).

% Balkan Mountains (Stara Planina)
cave("Temnata Dupka", "Iskar Gorge, Balkan Mountains", "Northwest Bulgaria", "near Lakatnik", "karst, deep cave system", 11500, 150).
cave("Duhlata Cave", "Vitosha Mountain", "Western Bulgaria", "Bosnek village, Pernik", "longest cave in Bulgaria", 18720, 53).
cave("Kozarnika Cave", "Balkan foothills", "Northwest Bulgaria", "near Belogradchik", "prehistoric site", 210, 60).

% Rila Mountains
cave("Zmeyova Dupka", "Rila Mountains", "Southwest Bulgaria", "near Dupnitsa", "karst, vertical cave", 500, 110).

% ======================================
% Helper predicates
% ======================================

% Location queries
located_in(Name, Location) :- cave(Name, _, _, Location, _, _, _).
in_region(Name, Region) :- cave(Name, _, Region, _, _, _, _).
in_mountain(Name, MountainRange) :- cave(Name, MountainRange, _, _, _, _, _).

% Type queries
cave_type(Name, Type) :- cave(Name, _, _, _, Type, _, _).

% Size queries
cave_length(Name, Length) :- cave(Name, _, _, _, _, Length, _).
cave_depth(Name, Depth) :- cave(Name, _, _, _, _, _, Depth).

% ======================================
% Aggregated queries
% ======================================

% List caves by mountain range
caves_by_mountain(Mountain, Name) :- cave(Name, Mountain, _, _, _, _, _).

% List caves by region
caves_by_region(Region, Name) :- cave(Name, _, Region, _, _, _, _).

% List caves by type
caves_by_type(Type, Name) :- cave(Name, _, _, _, Type, _, _).

% List caves by size
caves_longer_than(Meters, Name) :- cave(Name, _, _, _, _, Length, _), Length > Meters.
caves_deeper_than(Meters, Name) :- cave(Name, _, _, _, _, _, Depth), Depth > Meters.

% ======================================
% Full details
% ======================================

cave_details(Name, Details) :-
    cave(Name, Mountain, Region, Location, Type, Length, Depth),
    format(string(Details),
        "=== ~w ===~nMountain range: ~w~nRegion: ~w~nLocation: ~w~nType: ~w~nLength: ~w m~nDepth: ~w m~n",
        [Name, Mountain, Region, Location, Type, Length, Depth]).

