% ======================================
% Birds of Bulgaria
% bird(Name, Type, Habitat, Migration, Notes)
% ======================================

bird("White Stork", "Waterfowl", "Open fields, wetlands", "Migratory", "Famous for nesting on rooftops").
bird("European Robin", "Songbird", "Forests, gardens", "Resident", "Known for its red breast and singing").
bird("Common Buzzard", "Raptor", "Woodlands, farmlands", "Resident", "Medium-sized bird of prey").
bird("Peregrine Falcon", "Raptor", "Cliffs, urban areas", "Migratory", "Fastest bird in the world").
bird("Mute Swan", "Waterfowl", "Lakes, rivers", "Migratory", "Large white swan with orange beak").
bird("Great Spotted Woodpecker", "Woodpecker", "Forests", "Resident", "Distinct black-and-white plumage with red belly").
bird("Barn Swallow", "Songbird", "Open countryside, villages", "Migratory", "Recognizable by long forked tail and agile flight").
bird("Eurasian Eagle-Owl", "Raptor", "Mountains, cliffs", "Resident", "Large nocturnal owl with orange eyes").
bird("Common Kingfisher", "Waterfowl", "Rivers, lakes", "Resident", "Small bright blue and orange bird").
bird("Grey Heron", "Waterfowl", "Wetlands, rivers", "Resident", "Large wading bird with long neck").

% Additional examples
bird("Little Owl", "Raptor", "Farmlands, villages", "Resident", "Small nocturnal owl").
bird("European Goldfinch", "Songbird", "Fields, gardens", "Resident", "Colorful red, white, and yellow plumage").
bird("Black Kite", "Raptor", "Open landscapes", "Migratory", "Soaring scavenger bird").
bird("Common Cuckoo", "Songbird", "Forests, wetlands", "Migratory", "Famous for laying eggs in other birds' nests").
bird("White-tailed Eagle", "Raptor", "Rivers, lakes, coasts", "Resident", "Large eagle with white tail, endangered species in Bulgaria").

% ======================================
% Helper predicates
% ======================================

% Habitat queries
lives_in(Name, Habitat) :- bird(Name, _, Habitat, _, _).

% Type queries
bird_type(Name, Type) :- bird(Name, Type, _, _, _).

% Migration queries
migration_status(Name, Migration) :- bird(Name, _, _, Migration, _).

% Notes
bird_notes(Name, Notes) :- bird(Name, _, _, _, Notes).

% ======================================
% Aggregated queries
% ======================================

% Birds by type
birds_by_type(Type, Name) :- bird(Name, Type, _, _, _).

% Birds by habitat
birds_by_habitat(Habitat, Name) :- bird(Name, _, Habitat, _, _).

% Birds by migration
birds_by_migration(Migration, Name) :- bird(Name, _, _, Migration, _).

% ======================================
% Full details
% ======================================

bird_details(Name, Details) :-
    bird(Name, Type, Habitat, Migration, Notes),
    format(string(Details),
        "=== ~w ===~nType: ~w~nHabitat: ~w~nMigration: ~w~nNotes: ~w~n",
        [Name, Type, Habitat, Migration, Notes]).
