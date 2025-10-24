:- encoding(utf8).

% === ARTÍCULOS ===
% articulo(idioma, articulo, genero, numero)

% Español:
articulo(esp, el, masc, sing).
articulo(esp, la, fem, sing).
articulo(esp, los, masc, plur).
articulo(esp, las, fem, plur).
articulo(esp, un, masc, sing).
articulo(esp, una, fem, sing).
articulo(esp, unos, masc, plur).
articulo(esp, unas, fem, plur).

% Inglés:
articulo(eng, the, _, _).
articulo(eng, a, _, sing).
articulo(eng, an, _, sing).
articulo(eng, some, _, plur).

% Traducción de artículos (español -> inglés)
traducir_articulo(el, the).
traducir_articulo(la, the).
traducir_articulo(los, the).
traducir_articulo(las, the).
traducir_articulo(un, a).
traducir_articulo(una, a).
traducir_articulo(unos, some).
traducir_articulo(unas, some).

% Traducción de artículos (inglés -> español)
% Nota: "the" requiere contexto de género/número del sustantivo
traducir_articulo_ctx(the, el, masc, sing).
traducir_articulo_ctx(the, la, fem, sing).
traducir_articulo_ctx(the, los, masc, plur).
traducir_articulo_ctx(the, las, fem, plur).
traducir_articulo_ctx(a, un, masc, sing).
traducir_articulo_ctx(a, una, fem, sing).
traducir_articulo_ctx(an, un, masc, sing).
traducir_articulo_ctx(an, una, fem, sing).
traducir_articulo_ctx(some, unos, masc, plur).
traducir_articulo_ctx(some, unas, fem, plur).

% === SUSTANTIVOS ===
% sustantivo(idioma, sustantivo, genero, numero)

% Español:
sustantivo(esp, perro, masc, sing).
sustantivo(esp, perros, masc, plur).
sustantivo(esp, gato, masc, sing).
sustantivo(esp, gatos, masc, plur).
sustantivo(esp, casa, fem, sing).
sustantivo(esp, casas, fem, plur).
sustantivo(esp, libro, masc, sing).
sustantivo(esp, libros, masc, plur).
sustantivo(esp, niño, masc, sing).
sustantivo(esp, niños, masc, plur).
sustantivo(esp, niña, fem, sing).
sustantivo(esp, niñas, fem, plur).
sustantivo(esp, mesa, fem, sing).
sustantivo(esp, mesas, fem, plur).
sustantivo(esp, silla, fem, sing).
sustantivo(esp, sillas, fem, plur).
sustantivo(esp, árbol, masc, sing).
sustantivo(esp, árboles, masc, plur).
sustantivo(esp, flor, fem, sing).
sustantivo(esp, flores, fem, plur).
sustantivo(esp, coche, masc, sing).
sustantivo(esp, coches, masc, plur).
sustantivo(esp, puerta, fem, sing).
sustantivo(esp, puertas, fem, plur).
sustantivo(esp, ventana, fem, sing).
sustantivo(esp, ventanas, fem, plur).
sustantivo(esp, hombre, masc, sing).
sustantivo(esp, hombres, masc, plur).
sustantivo(esp, mujer, fem, sing).
sustantivo(esp, mujeres, fem, plur).
sustantivo(esp, agua, fem, sing).
sustantivo(esp, comida, fem, sing).
sustantivo(esp, pan, masc, sing).
sustantivo(esp, leche, fem, sing).
sustantivo(esp, café, masc, sing).
sustantivo(esp, té, masc, sing).
sustantivo(esp, ciudad, fem, sing).
sustantivo(esp, ciudades, fem, plur).
sustantivo(esp, país, masc, sing).
sustantivo(esp, países, masc, plur).
sustantivo(esp, mundo, masc, sing).
sustantivo(esp, día, masc, sing).
sustantivo(esp, días, masc, plur).
sustantivo(esp, noche, fem, sing).
sustantivo(esp, noches, fem, plur).
sustantivo(esp, mañana, fem, sing).
sustantivo(esp, tarde, fem, sing).
sustantivo(esp, amigo, masc, sing).
sustantivo(esp, amigos, masc, plur).
sustantivo(esp, amiga, fem, sing).
sustantivo(esp, amigas, fem, plur).
sustantivo(esp, escuela, fem, sing).
sustantivo(esp, escuelas, fem, plur).
sustantivo(esp, universidad, fem, sing).
sustantivo(esp, universidades, fem, plur).
sustantivo(esp, profesor, masc, sing).
sustantivo(esp, profesores, masc, plur).
sustantivo(esp, profesora, fem, sing).
sustantivo(esp, profesoras, fem, plur).
sustantivo(esp, estudiante, masc, sing).
sustantivo(esp, estudiantes, masc, plur).
sustantivo(esp, clase, fem, sing).
sustantivo(esp, clases, fem, plur).
sustantivo(esp, computadora, fem, sing).
sustantivo(esp, computadoras, fem, plur).
sustantivo(esp, teléfono, masc, sing).
sustantivo(esp, teléfonos, masc, plur).
sustantivo(esp, tiempo, masc, sing).
sustantivo(esp, año, masc, sing).
sustantivo(esp, años, masc, plur).
sustantivo(esp, mes, masc, sing).
sustantivo(esp, meses, masc, plur).
sustantivo(esp, semana, fem, sing).
sustantivo(esp, semanas, fem, plur).
sustantivo(esp, sol, masc, sing).
sustantivo(esp, luna, fem, sing).
sustantivo(esp, estrella, fem, sing).
sustantivo(esp, estrellas, fem, plur).
sustantivo(esp, cielo, masc, sing).
sustantivo(esp, tierra, fem, sing).
sustantivo(esp, mar, masc, sing).
sustantivo(esp, río, masc, sing).
sustantivo(esp, ríos, masc, plur).
sustantivo(esp, montaña, fem, sing).
sustantivo(esp, montañas, fem, plur).
sustantivo(esp, playa, fem, sing).
sustantivo(esp, playas, fem, plur).
sustantivo(esp, parque, masc, sing).
sustantivo(esp, parques, masc, plur).
sustantivo(esp, jardín, masc, sing).
sustantivo(esp, jardines, masc, plur).
sustantivo(esp, animal, masc, sing).
sustantivo(esp, animales, masc, plur).
sustantivo(esp, pájaro, masc, sing).
sustantivo(esp, pájaros, masc, plur).
sustantivo(esp, pez, masc, sing).
sustantivo(esp, peces, masc, plur).
sustantivo(esp, caballo, masc, sing).
sustantivo(esp, caballos, masc, plur).
sustantivo(esp, vaca, fem, sing).
sustantivo(esp, vacas, fem, plur).

% Inglés:
sustantivo(eng, dog, _, sing).
sustantivo(eng, dogs, _, plur).
sustantivo(eng, cat, _, sing).
sustantivo(eng, cats, _, plur).
sustantivo(eng, house, _, sing).
sustantivo(eng, houses, _, plur).
sustantivo(eng, book, _, sing).
sustantivo(eng, books, _, plur).
sustantivo(eng, boy, _, sing).
sustantivo(eng, boys, _, plur).
sustantivo(eng, girl, _, sing).
sustantivo(eng, girls, _, plur).
sustantivo(eng, table, _, sing).
sustantivo(eng, tables, _, plur).
sustantivo(eng, chair, _, sing).
sustantivo(eng, chairs, _, plur).
sustantivo(eng, tree, _, sing).
sustantivo(eng, trees, _, plur).
sustantivo(eng, flower, _, sing).
sustantivo(eng, flowers, _, plur).
sustantivo(eng, car, _, sing).
sustantivo(eng, cars, _, plur).
sustantivo(eng, door, _, sing).
sustantivo(eng, doors, _, plur).
sustantivo(eng, window, _, sing).
sustantivo(eng, windows, _, plur).
sustantivo(eng, man, _, sing).
sustantivo(eng, men, _, plur).
sustantivo(eng, woman, _, sing).
sustantivo(eng, women, _, plur).
sustantivo(eng, water, _, sing).
sustantivo(eng, food, _, sing).
sustantivo(eng, bread, _, sing).
sustantivo(eng, milk, _, sing).
sustantivo(eng, coffee, _, sing).
sustantivo(eng, tea, _, sing).
sustantivo(eng, city, _, sing).
sustantivo(eng, cities, _, plur).
sustantivo(eng, country, _, sing).
sustantivo(eng, countries, _, plur).
sustantivo(eng, world, _, sing).
sustantivo(eng, day, _, sing).
sustantivo(eng, days, _, plur).
sustantivo(eng, night, _, sing).
sustantivo(eng, nights, _, plur).
sustantivo(eng, morning, _, sing).
sustantivo(eng, afternoon, _, sing).
sustantivo(eng, friend, _, sing).
sustantivo(eng, friends, _, plur).
sustantivo(eng, school, _, sing).
sustantivo(eng, schools, _, plur).
sustantivo(eng, university, _, sing).
sustantivo(eng, universities, _, plur).
sustantivo(eng, teacher, _, sing).
sustantivo(eng, teachers, _, plur).
sustantivo(eng, student, _, sing).
sustantivo(eng, students, _, plur).
sustantivo(eng, class, _, sing).
sustantivo(eng, classes, _, plur).
sustantivo(eng, computer, _, sing).
sustantivo(eng, computers, _, plur).
sustantivo(eng, phone, _, sing).
sustantivo(eng, phones, _, plur).
sustantivo(eng, time, _, sing).
sustantivo(eng, year, _, sing).
sustantivo(eng, years, _, plur).
sustantivo(eng, month, _, sing).
sustantivo(eng, months, _, plur).
sustantivo(eng, week, _, sing).
sustantivo(eng, weeks, _, plur).
sustantivo(eng, sun, _, sing).
sustantivo(eng, moon, _, sing).
sustantivo(eng, star, _, sing).
sustantivo(eng, stars, _, plur).
sustantivo(eng, sky, _, sing).
sustantivo(eng, earth, _, sing).
sustantivo(eng, sea, _, sing).
sustantivo(eng, river, _, sing).
sustantivo(eng, rivers, _, plur).
sustantivo(eng, mountain, _, sing).
sustantivo(eng, mountains, _, plur).
sustantivo(eng, beach, _, sing).
sustantivo(eng, beaches, _, plur).
sustantivo(eng, park, _, sing).
sustantivo(eng, parks, _, plur).
sustantivo(eng, garden, _, sing).
sustantivo(eng, gardens, _, plur).
sustantivo(eng, animal, _, sing).
sustantivo(eng, animals, _, plur).
sustantivo(eng, bird, _, sing).
sustantivo(eng, birds, _, plur).
sustantivo(eng, fish, _, sing).
sustantivo(eng, fishes, _, plur).
sustantivo(eng, horse, _, sing).
sustantivo(eng, horses, _, plur).
sustantivo(eng, cow, _, sing).
sustantivo(eng, cows, _, plur).

% Traducción de sustantivos (español -> inglés)
traducir_sustantivo(perro, dog).
traducir_sustantivo(perros, dogs).
traducir_sustantivo(gato, cat).
traducir_sustantivo(gatos, cats).
traducir_sustantivo(casa, house).
traducir_sustantivo(casas, houses).
traducir_sustantivo(libro, book).
traducir_sustantivo(libros, books).
traducir_sustantivo(niño, boy).
traducir_sustantivo(niños, boys).
traducir_sustantivo(niña, girl).
traducir_sustantivo(niñas, girls).
traducir_sustantivo(mesa, table).
traducir_sustantivo(mesas, tables).
traducir_sustantivo(silla, chair).
traducir_sustantivo(sillas, chairs).
traducir_sustantivo(árbol, tree).
traducir_sustantivo(árboles, trees).
traducir_sustantivo(flor, flower).
traducir_sustantivo(flores, flowers).
traducir_sustantivo(coche, car).
traducir_sustantivo(coches, cars).
traducir_sustantivo(puerta, door).
traducir_sustantivo(puertas, doors).
traducir_sustantivo(ventana, window).
traducir_sustantivo(ventanas, windows).
traducir_sustantivo(hombre, man).
traducir_sustantivo(hombres, men).
traducir_sustantivo(mujer, woman).
traducir_sustantivo(mujeres, women).
traducir_sustantivo(agua, water).
traducir_sustantivo(comida, food).
traducir_sustantivo(pan, bread).
traducir_sustantivo(leche, milk).
traducir_sustantivo(café, coffee).
traducir_sustantivo(té, tea).
traducir_sustantivo(ciudad, city).
traducir_sustantivo(ciudades, cities).
traducir_sustantivo(país, country).
traducir_sustantivo(países, countries).
traducir_sustantivo(mundo, world).
traducir_sustantivo(día, day).
traducir_sustantivo(días, days).
traducir_sustantivo(noche, night).
traducir_sustantivo(noches, nights).
traducir_sustantivo(mañana, morning).
traducir_sustantivo(tarde, afternoon).
traducir_sustantivo(amigo, friend).
traducir_sustantivo(amigos, friends).
traducir_sustantivo(amiga, friend).
traducir_sustantivo(amigas, friends).
traducir_sustantivo(escuela, school).
traducir_sustantivo(escuelas, schools).
traducir_sustantivo(universidad, university).
traducir_sustantivo(universidades, universities).
traducir_sustantivo(profesor, teacher).
traducir_sustantivo(profesores, teachers).
traducir_sustantivo(profesora, teacher).
traducir_sustantivo(profesoras, teachers).
traducir_sustantivo(estudiante, student).
traducir_sustantivo(estudiantes, students).
traducir_sustantivo(clase, class).
traducir_sustantivo(clases, classes).
traducir_sustantivo(computadora, computer).
traducir_sustantivo(computadoras, computers).
traducir_sustantivo(teléfono, phone).
traducir_sustantivo(teléfonos, phones).
traducir_sustantivo(tiempo, time).
traducir_sustantivo(año, year).
traducir_sustantivo(años, years).
traducir_sustantivo(mes, month).
traducir_sustantivo(meses, months).
traducir_sustantivo(semana, week).
traducir_sustantivo(semanas, weeks).
traducir_sustantivo(sol, sun).
traducir_sustantivo(luna, moon).
traducir_sustantivo(estrella, star).
traducir_sustantivo(estrellas, stars).
traducir_sustantivo(cielo, sky).
traducir_sustantivo(tierra, earth).
traducir_sustantivo(mar, sea).
traducir_sustantivo(río, river).
traducir_sustantivo(ríos, rivers).
traducir_sustantivo(montaña, mountain).
traducir_sustantivo(montañas, mountains).
traducir_sustantivo(playa, beach).
traducir_sustantivo(playas, beaches).
traducir_sustantivo(parque, park).
traducir_sustantivo(parques, parks).
traducir_sustantivo(jardín, garden).
traducir_sustantivo(jardines, gardens).
traducir_sustantivo(animal, animal).
traducir_sustantivo(animales, animals).
traducir_sustantivo(pájaro, bird).
traducir_sustantivo(pájaros, birds).
traducir_sustantivo(pez, fish).
traducir_sustantivo(peces, fishes).
traducir_sustantivo(caballo, horse).
traducir_sustantivo(caballos, horses).
traducir_sustantivo(vaca, cow).
traducir_sustantivo(vacas, cows).

% Traducción inversa (inglés -> español)
traducir_sustantivo(dog, perro).
traducir_sustantivo(dogs, perros).
traducir_sustantivo(cat, gato).
traducir_sustantivo(cats, gatos).
traducir_sustantivo(house, casa).
traducir_sustantivo(houses, casas).
traducir_sustantivo(book, libro).
traducir_sustantivo(books, libros).
traducir_sustantivo(boy, niño).
traducir_sustantivo(boys, niños).
traducir_sustantivo(girl, niña).
traducir_sustantivo(girls, niñas).
traducir_sustantivo(table, mesa).
traducir_sustantivo(tables, mesas).
traducir_sustantivo(chair, silla).
traducir_sustantivo(chairs, sillas).
traducir_sustantivo(tree, árbol).
traducir_sustantivo(trees, árboles).
traducir_sustantivo(flower, flor).
traducir_sustantivo(flowers, flores).
traducir_sustantivo(car, coche).
traducir_sustantivo(cars, coches).
traducir_sustantivo(door, puerta).
traducir_sustantivo(doors, puertas).
traducir_sustantivo(window, ventana).
traducir_sustantivo(windows, ventanas).
traducir_sustantivo(man, hombre).
traducir_sustantivo(men, hombres).
traducir_sustantivo(woman, mujer).
traducir_sustantivo(women, mujeres).
traducir_sustantivo(water, agua).
traducir_sustantivo(food, comida).
traducir_sustantivo(bread, pan).
traducir_sustantivo(milk, leche).
traducir_sustantivo(coffee, café).
traducir_sustantivo(tea, té).
traducir_sustantivo(city, ciudad).
traducir_sustantivo(cities, ciudades).
traducir_sustantivo(country, país).
traducir_sustantivo(countries, países).
traducir_sustantivo(world, mundo).
traducir_sustantivo(day, día).
traducir_sustantivo(days, días).
traducir_sustantivo(night, noche).
traducir_sustantivo(nights, noches).
traducir_sustantivo(morning, mañana).
traducir_sustantivo(afternoon, tarde).
traducir_sustantivo(friend, amigo).
traducir_sustantivo(friends, amigos).
traducir_sustantivo(school, escuela).
traducir_sustantivo(schools, escuelas).
traducir_sustantivo(university, universidad).
traducir_sustantivo(universities, universidades).
traducir_sustantivo(teacher, profesor).
traducir_sustantivo(teachers, profesores).
traducir_sustantivo(student, estudiante).
traducir_sustantivo(students, estudiantes).
traducir_sustantivo(class, clase).
traducir_sustantivo(classes, clases).
traducir_sustantivo(computer, computadora).
traducir_sustantivo(computers, computadoras).
traducir_sustantivo(phone, teléfono).
traducir_sustantivo(phones, teléfonos).
traducir_sustantivo(time, tiempo).
traducir_sustantivo(year, año).
traducir_sustantivo(years, años).
traducir_sustantivo(month, mes).
traducir_sustantivo(months, meses).
traducir_sustantivo(week, semana).
traducir_sustantivo(weeks, semanas).
traducir_sustantivo(sun, sol).
traducir_sustantivo(moon, luna).
traducir_sustantivo(star, estrella).
traducir_sustantivo(stars, estrellas).
traducir_sustantivo(sky, cielo).
traducir_sustantivo(earth, tierra).
traducir_sustantivo(sea, mar).
traducir_sustantivo(river, río).
traducir_sustantivo(rivers, ríos).
traducir_sustantivo(mountain, montaña).
traducir_sustantivo(mountains, montañas).
traducir_sustantivo(beach, playa).
traducir_sustantivo(beaches, playas).
traducir_sustantivo(park, parque).
traducir_sustantivo(parks, parques).
traducir_sustantivo(garden, jardín).
traducir_sustantivo(gardens, jardines).
traducir_sustantivo(animal, animal).
traducir_sustantivo(animals, animales).
traducir_sustantivo(bird, pájaro).
traducir_sustantivo(birds, pájaros).
traducir_sustantivo(fish, pez).
traducir_sustantivo(fishes, peces).
traducir_sustantivo(horse, caballo).
traducir_sustantivo(horses, caballos).
traducir_sustantivo(cow, vaca).
traducir_sustantivo(cows, vacas).

% === ADJETIVOS ===
% adjetivo(idioma, adjetivo, genero, numero)

% Español:
adjetivo(esp, grande, _, _).
adjetivo(esp, pequeño, masc, _).
adjetivo(esp, pequeña, fem, _).
adjetivo(esp, rojo, masc, _).
adjetivo(esp, roja, fem, _).
adjetivo(esp, bonito, masc, _).
adjetivo(esp, bonita, fem, _).
adjetivo(esp, feo, masc, _).
adjetivo(esp, fea, fem, _).
adjetivo(esp, alto, masc, _).
adjetivo(esp, alta, fem, _).
adjetivo(esp, bajo, masc, _).
adjetivo(esp, baja, fem, _).
adjetivo(esp, nuevo, masc, _).
adjetivo(esp, nueva, fem, _).
adjetivo(esp, viejo, masc, _).
adjetivo(esp, vieja, fem, _).
adjetivo(esp, joven, _, _).
adjetivo(esp, feliz, _, _).
adjetivo(esp, triste, _, _).
adjetivo(esp, rápido, masc, _).
adjetivo(esp, rápida, fem, _).
adjetivo(esp, lento, masc, _).
adjetivo(esp, lenta, fem, _).
adjetivo(esp, bueno, masc, _).
adjetivo(esp, buena, fem, _).
adjetivo(esp, malo, masc, _).
adjetivo(esp, mala, fem, _).
adjetivo(esp, blanco, masc, _).
adjetivo(esp, blanca, fem, _).
adjetivo(esp, negro, masc, _).
adjetivo(esp, negra, fem, _).
adjetivo(esp, azul, _, _).
adjetivo(esp, verde, _, _).
adjetivo(esp, amarillo, masc, _).
adjetivo(esp, amarilla, fem, _).
adjetivo(esp, inteligente, _, _).
adjetivo(esp, divertido, masc, _).
adjetivo(esp, divertida, fem, _).
adjetivo(esp, aburrido, masc, _).
adjetivo(esp, aburrida, fem, _).

% Inglés:
adjetivo(eng, big, _, _).
adjetivo(eng, small, _, _).
adjetivo(eng, red, _, _).
adjetivo(eng, beautiful, _, _).
adjetivo(eng, ugly, _, _).
adjetivo(eng, tall, _, _).
adjetivo(eng, short, _, _).
adjetivo(eng, new, _, _).
adjetivo(eng, old, _, _).
adjetivo(eng, young, _, _).
adjetivo(eng, happy, _, _).
adjetivo(eng, sad, _, _).
adjetivo(eng, fast, _, _).
adjetivo(eng, slow, _, _).
adjetivo(eng, good, _, _).
adjetivo(eng, bad, _, _).
adjetivo(eng, white, _, _).
adjetivo(eng, black, _, _).
adjetivo(eng, blue, _, _).
adjetivo(eng, green, _, _).
adjetivo(eng, yellow, _, _).
adjetivo(eng, intelligent, _, _).
adjetivo(eng, fun, _, _).
adjetivo(eng, boring, _, _).

% Traducción de adjetivos
traducir_adjetivo(grande, big).
traducir_adjetivo(pequeño, small).
traducir_adjetivo(pequeña, small).
traducir_adjetivo(rojo, red).
traducir_adjetivo(roja, red).
traducir_adjetivo(bonito, beautiful).
traducir_adjetivo(bonita, beautiful).
traducir_adjetivo(feo, ugly).
traducir_adjetivo(fea, ugly).
traducir_adjetivo(alto, tall).
traducir_adjetivo(alta, tall).
traducir_adjetivo(bajo, short).
traducir_adjetivo(baja, short).
traducir_adjetivo(nuevo, new).
traducir_adjetivo(nueva, new).
traducir_adjetivo(viejo, old).
traducir_adjetivo(vieja, old).
traducir_adjetivo(joven, young).
traducir_adjetivo(feliz, happy).
traducir_adjetivo(triste, sad).
traducir_adjetivo(rápido, fast).
traducir_adjetivo(rápida, fast).
traducir_adjetivo(lento, slow).
traducir_adjetivo(lenta, slow).
traducir_adjetivo(bueno, good).
traducir_adjetivo(buena, good).
traducir_adjetivo(malo, bad).
traducir_adjetivo(mala, bad).
traducir_adjetivo(blanco, white).
traducir_adjetivo(blanca, white).
traducir_adjetivo(negro, black).
traducir_adjetivo(negra, black).
traducir_adjetivo(azul, blue).
traducir_adjetivo(verde, green).
traducir_adjetivo(amarillo, yellow).
traducir_adjetivo(amarilla, yellow).
traducir_adjetivo(inteligente, intelligent).
traducir_adjetivo(divertido, fun).
traducir_adjetivo(divertida, fun).
traducir_adjetivo(aburrido, boring).
traducir_adjetivo(aburrida, boring).

% Traducción inversa
traducir_adjetivo(big, grande).
traducir_adjetivo(small, pequeño).
traducir_adjetivo(red, rojo).
traducir_adjetivo(beautiful, bonito).
traducir_adjetivo(ugly, feo).
traducir_adjetivo(tall, alto).
traducir_adjetivo(short, bajo).
traducir_adjetivo(new, nuevo).
traducir_adjetivo(old, viejo).
traducir_adjetivo(young, joven).
traducir_adjetivo(happy, feliz).
traducir_adjetivo(sad, triste).
traducir_adjetivo(fast, rápido).
traducir_adjetivo(slow, lento).
traducir_adjetivo(good, bueno).
traducir_adjetivo(bad, malo).
traducir_adjetivo(white, blanco).
traducir_adjetivo(black, negro).
traducir_adjetivo(blue, azul).
traducir_adjetivo(green, verde).
traducir_adjetivo(yellow, amarillo).
traducir_adjetivo(intelligent, inteligente).
traducir_adjetivo(fun, divertido).
traducir_adjetivo(boring, aburrido).

% === VERBOS (PRESENTE) ===
% verbo(idioma, verbo, persona, numero)

% Español:
verbo(esp, corro, primera, sing).
verbo(esp, corres, segunda, sing).
verbo(esp, corre, tercera, sing).
verbo(esp, corremos, primera, plur).
verbo(esp, corren, tercera, plur).

verbo(esp, como, primera, sing).
verbo(esp, comes, segunda, sing).
verbo(esp, come, tercera, sing).
verbo(esp, comemos, primera, plur).
verbo(esp, comen, tercera, plur).

verbo(esp, leo, primera, sing).
verbo(esp, lees, segunda, sing).
verbo(esp, lee, tercera, sing).
verbo(esp, leemos, primera, plur).
verbo(esp, leen, tercera, plur).

verbo(esp, escribo, primera, sing).
verbo(esp, escribes, segunda, sing).
verbo(esp, escribe, tercera, sing).
verbo(esp, escribimos, primera, plur).
verbo(esp, escriben, tercera, plur).

verbo(esp, hablo, primera, sing).
verbo(esp, hablas, segunda, sing).
verbo(esp, habla, tercera, sing).
verbo(esp, hablamos, primera, plur).
verbo(esp, hablan, tercera, plur).

verbo(esp, vivo, primera, sing).
verbo(esp, vives, segunda, sing).
verbo(esp, vive, tercera, sing).
verbo(esp, vivimos, primera, plur).
verbo(esp, viven, tercera, plur).

verbo(esp, trabajo, primera, sing).
verbo(esp, trabajas, segunda, sing).
verbo(esp, trabaja, tercera, sing).
verbo(esp, trabajamos, primera, plur).
verbo(esp, trabajan, tercera, plur).

verbo(esp, estudio, primera, sing).
verbo(esp, estudias, segunda, sing).
verbo(esp, estudia, tercera, sing).
verbo(esp, estudiamos, primera, plur).
verbo(esp, estudian, tercera, plur).

verbo(esp, juego, primera, sing).
verbo(esp, juegas, segunda, sing).
verbo(esp, juega, tercera, sing).
verbo(esp, jugamos, primera, plur).
verbo(esp, juegan, tercera, plur).

verbo(esp, duermo, primera, sing).
verbo(esp, duermes, segunda, sing).
verbo(esp, duerme, tercera, sing).
verbo(esp, dormimos, primera, plur).
verbo(esp, duermen, tercera, plur).

verbo(esp, camino, primera, sing).
verbo(esp, caminas, segunda, sing).
verbo(esp, camina, tercera, sing).
verbo(esp, caminamos, primera, plur).
verbo(esp, caminan, tercera, plur).

verbo(esp, bebo, primera, sing).
verbo(esp, bebes, segunda, sing).
verbo(esp, bebe, tercera, sing).
verbo(esp, bebemos, primera, plur).
verbo(esp, beben, tercera, plur).

verbo(esp, veo, primera, sing).
verbo(esp, ves, segunda, sing).
verbo(esp, ve, tercera, sing).
verbo(esp, vemos, primera, plur).
verbo(esp, ven, tercera, plur).

verbo(esp, escucho, primera, sing).
verbo(esp, escuchas, segunda, sing).
verbo(esp, escucha, tercera, sing).
verbo(esp, escuchamos, primera, plur).
verbo(esp, escuchan, tercera, plur).

verbo(esp, salto, primera, sing).
verbo(esp, saltas, segunda, sing).
verbo(esp, salta, tercera, sing).
verbo(esp, saltamos, primera, plur).
verbo(esp, saltan, tercera, plur).

verbo(esp, amo, primera, sing).
verbo(esp, amas, segunda, sing).
verbo(esp, ama, tercera, sing).
verbo(esp, amamos, primera, plur).
verbo(esp, aman, tercera, plur).

verbo(esp, enseño, primera, sing).
verbo(esp, enseñas, segunda, sing).
verbo(esp, enseña, tercera, sing).
verbo(esp, enseñamos, primera, plur).
verbo(esp, enseñan, tercera, plur).

verbo(esp, aprendo, primera, sing).
verbo(esp, aprendes, segunda, sing).
verbo(esp, aprende, tercera, sing).
verbo(esp, aprendemos, primera, plur).
verbo(esp, aprenden, tercera, plur).

verbo(esp, canto, primera, sing).
verbo(esp, cantas, segunda, sing).
verbo(esp, canta, tercera, sing).
verbo(esp, cantamos, primera, plur).
verbo(esp, cantan, tercera, plur).

verbo(esp, bailo, primera, sing).
verbo(esp, bailas, segunda, sing).
verbo(esp, baila, tercera, sing).
verbo(esp, bailamos, primera, plur).
verbo(esp, bailan, tercera, plur).

verbo(esp, viajo, primera, sing).
verbo(esp, viajas, segunda, sing).
verbo(esp, viaja, tercera, sing).
verbo(esp, viajamos, primera, plur).
verbo(esp, viajan, tercera, plur).

verbo(esp, nado, primera, sing).
verbo(esp, nadas, segunda, sing).
verbo(esp, nada, tercera, sing).
verbo(esp, nadamos, primera, plur).
verbo(esp, nadan, tercera, plur).

verbo(esp, cocino, primera, sing).
verbo(esp, cocinas, segunda, sing).
verbo(esp, cocina, tercera, sing).
verbo(esp, cocinamos, primera, plur).
verbo(esp, cocinan, tercera, plur).

verbo(esp, estoy, primera, sing).
verbo(esp, estás, segunda, sing).
verbo(esp, está, tercera, sing).
verbo(esp, estamos, primera, plur).
verbo(esp, están, tercera, plur).

verbo(esp, tengo, primera, sing).
verbo(esp, tienes, segunda, sing).
verbo(esp, tiene, tercera, sing).
verbo(esp, tenemos, primera, plur).
verbo(esp, tienen, tercera, plur).

% Inglés:
verbo(eng, run, _, _).
verbo(eng, runs, tercera, sing).
verbo(eng, eat, _, _).
verbo(eng, eats, tercera, sing).
verbo(eng, read, _, _).
verbo(eng, reads, tercera, sing).
verbo(eng, write, _, _).
verbo(eng, writes, tercera, sing).
verbo(eng, speak, _, _).
verbo(eng, speaks, tercera, sing).
verbo(eng, live, _, _).
verbo(eng, lives, tercera, sing).
verbo(eng, work, _, _).
verbo(eng, works, tercera, sing).
verbo(eng, study, _, _).
verbo(eng, studies, tercera, sing).
verbo(eng, play, _, _).
verbo(eng, plays, tercera, sing).
verbo(eng, sleep, _, _).
verbo(eng, sleeps, tercera, sing).
verbo(eng, walk, _, _).
verbo(eng, walks, tercera, sing).
verbo(eng, drink, _, _).
verbo(eng, drinks, tercera, sing).
verbo(eng, see, _, _).
verbo(eng, sees, tercera, sing).
verbo(eng, listen, _, _).
verbo(eng, listens, tercera, sing).
verbo(eng, jump, _, _).
verbo(eng, jumps, tercera, sing).
verbo(eng, love, _, _).
verbo(eng, loves, tercera, sing).
verbo(eng, teach, _, _).
verbo(eng, teaches, tercera, sing).
verbo(eng, learn, _, _).
verbo(eng, learns, tercera, sing).
verbo(eng, sing, _, _).
verbo(eng, sings, tercera, sing).
verbo(eng, dance, _, _).
verbo(eng, dances, tercera, sing).
verbo(eng, travel, _, _).
verbo(eng, travels, tercera, sing).
verbo(eng, swim, _, _).
verbo(eng, swims, tercera, sing).
verbo(eng, cook, _, _).
verbo(eng, cooks, tercera, sing).
verbo(eng, am, primera, sing).
verbo(eng, are, segunda, _).
verbo(eng, is, tercera, sing).
verbo(eng, have, _, _).
verbo(eng, has, tercera, sing).

% Traducción de verbos (español -> inglés)
traducir_verbo(corro, run).
traducir_verbo(corres, run).
traducir_verbo(corre, runs).
traducir_verbo(corremos, run).
traducir_verbo(corren, run).

traducir_verbo(como, eat).
traducir_verbo(comes, eat).
traducir_verbo(come, eats).
traducir_verbo(comemos, eat).
traducir_verbo(comen, eat).

traducir_verbo(leo, read).
traducir_verbo(lees, read).
traducir_verbo(lee, reads).
traducir_verbo(leemos, read).
traducir_verbo(leen, read).

traducir_verbo(escribo, write).
traducir_verbo(escribes, write).
traducir_verbo(escribe, writes).
traducir_verbo(escribimos, write).
traducir_verbo(escriben, write).

traducir_verbo(hablo, speak).
traducir_verbo(hablas, speak).
traducir_verbo(habla, speaks).
traducir_verbo(hablamos, speak).
traducir_verbo(hablan, speak).

traducir_verbo(vivo, live).
traducir_verbo(vives, live).
traducir_verbo(vive, lives).
traducir_verbo(vivimos, live).
traducir_verbo(viven, live).

traducir_verbo(trabajo, work).
traducir_verbo(trabajas, work).
traducir_verbo(trabaja, works).
traducir_verbo(trabajamos, work).
traducir_verbo(trabajan, work).

traducir_verbo(estudio, study).
traducir_verbo(estudias, study).
traducir_verbo(estudia, studies).
traducir_verbo(estudiamos, study).
traducir_verbo(estudian, study).

traducir_verbo(juego, play).
traducir_verbo(juegas, play).
traducir_verbo(juega, plays).
traducir_verbo(jugamos, play).
traducir_verbo(juegan, play).

traducir_verbo(duermo, sleep).
traducir_verbo(duermes, sleep).
traducir_verbo(duerme, sleeps).
traducir_verbo(dormimos, sleep).
traducir_verbo(duermen, sleep).

traducir_verbo(camino, walk).
traducir_verbo(caminas, walk).
traducir_verbo(camina, walks).
traducir_verbo(caminamos, walk).
traducir_verbo(caminan, walk).

traducir_verbo(bebo, drink).
traducir_verbo(bebes, drink).
traducir_verbo(bebe, drinks).
traducir_verbo(bebemos, drink).
traducir_verbo(beben, drink).

traducir_verbo(veo, see).
traducir_verbo(ves, see).
traducir_verbo(ve, sees).
traducir_verbo(vemos, see).
traducir_verbo(ven, see).

traducir_verbo(escucho, listen).
traducir_verbo(escuchas, listen).
traducir_verbo(escucha, listens).
traducir_verbo(escuchamos, listen).
traducir_verbo(escuchan, listen).

traducir_verbo(salto, jump).
traducir_verbo(saltas, jump).
traducir_verbo(salta, jumps).
traducir_verbo(saltamos, jump).
traducir_verbo(saltan, jump).

traducir_verbo(amo, love).
traducir_verbo(amas, love).
traducir_verbo(ama, loves).
traducir_verbo(amamos, love).
traducir_verbo(aman, love).

traducir_verbo(enseño, teach).
traducir_verbo(enseñas, teach).
traducir_verbo(enseña, teaches).
traducir_verbo(enseñamos, teach).
traducir_verbo(enseñan, teach).

traducir_verbo(aprendo, learn).
traducir_verbo(aprendes, learn).
traducir_verbo(aprende, learns).
traducir_verbo(aprendemos, learn).
traducir_verbo(aprenden, learn).

traducir_verbo(canto, sing).
traducir_verbo(cantas, sing).
traducir_verbo(canta, sings).
traducir_verbo(cantamos, sing).
traducir_verbo(cantan, sing).

traducir_verbo(bailo, dance).
traducir_verbo(bailas, dance).
traducir_verbo(baila, dances).
traducir_verbo(bailamos, dance).
traducir_verbo(bailan, dance).

traducir_verbo(viajo, travel).
traducir_verbo(viajas, travel).
traducir_verbo(viaja, travels).
traducir_verbo(viajamos, travel).
traducir_verbo(viajan, travel).

traducir_verbo(nado, swim).
traducir_verbo(nadas, swim).
traducir_verbo(nada, swims).
traducir_verbo(nadamos, swim).
traducir_verbo(nadan, swim).

traducir_verbo(cocino, cook).
traducir_verbo(cocinas, cook).
traducir_verbo(cocina, cooks).
traducir_verbo(cocinamos, cook).
traducir_verbo(cocinan, cook).

traducir_verbo(estoy, am).
traducir_verbo(estás, are).
traducir_verbo(está, is).
traducir_verbo(estamos, are).
traducir_verbo(están, are).

traducir_verbo(tengo, have).
traducir_verbo(tienes, have).
traducir_verbo(tiene, has).
traducir_verbo(tenemos, have).
traducir_verbo(tienen, have).

% Traducción inversa (inglés -> español)
traducir_verbo(run, corro).
traducir_verbo(runs, corre).
traducir_verbo(eat, como).
traducir_verbo(eats, come).
traducir_verbo(read, leo).
traducir_verbo(reads, lee).
traducir_verbo(write, escribo).
traducir_verbo(writes, escribe).
traducir_verbo(speak, hablo).
traducir_verbo(speaks, habla).
traducir_verbo(live, vivo).
traducir_verbo(lives, vive).
traducir_verbo(work, trabajo).
traducir_verbo(works, trabaja).
traducir_verbo(study, estudio).
traducir_verbo(studies, estudia).
traducir_verbo(play, juego).
traducir_verbo(plays, juega).
traducir_verbo(sleep, duermo).
traducir_verbo(sleeps, duerme).
traducir_verbo(walk, camino).
traducir_verbo(walks, camina).
traducir_verbo(drink, bebo).
traducir_verbo(drinks, bebe).
traducir_verbo(see, veo).
traducir_verbo(sees, ve).
traducir_verbo(listen, escucho).
traducir_verbo(listens, escucha).
traducir_verbo(jump, salto).
traducir_verbo(jumps, salta).
traducir_verbo(love, amo).
traducir_verbo(loves, ama).
traducir_verbo(teach, enseño).
traducir_verbo(teaches, enseña).
traducir_verbo(learn, aprendo).
traducir_verbo(learns, aprende).
traducir_verbo(sing, canto).
traducir_verbo(sings, canta).
traducir_verbo(dance, bailo).
traducir_verbo(dances, baila).
traducir_verbo(travel, viajo).
traducir_verbo(travels, viaja).
traducir_verbo(swim, nado).
traducir_verbo(swims, nada).
traducir_verbo(cook, cocino).
traducir_verbo(cooks, cocina).
traducir_verbo(am, estoy).
traducir_verbo(are, estás).
traducir_verbo(is, está).
traducir_verbo(have, tengo).
traducir_verbo(has, tiene).

% === PRONOMBRES ===
% pronombre(idioma, pronombre, persona, numero)

% Español:
pronombre(esp, yo, primera, sing).
pronombre(esp, tú, segunda, sing).
pronombre(esp, él, tercera, sing).
pronombre(esp, ella, tercera, sing).
pronombre(esp, nosotros, primera, plur).
pronombre(esp, nosotras, primera, plur).
pronombre(esp, vosotros, segunda, plur).
pronombre(esp, vosotras, segunda, plur).
pronombre(esp, ellos, tercera, plur).
pronombre(esp, ellas, tercera, plur).
pronombre(esp, usted, segunda, sing).
pronombre(esp, ustedes, segunda, plur).

% Inglés:
pronombre(eng, i, primera, sing).
pronombre(eng, you, segunda, _).
pronombre(eng, he, tercera, sing).
pronombre(eng, she, tercera, sing).
pronombre(eng, it, tercera, sing).
pronombre(eng, we, primera, plur).
pronombre(eng, they, tercera, plur).

% Traducción de pronombres
traducir_pronombre(yo, i).
traducir_pronombre(tú, you).
traducir_pronombre(él, he).
traducir_pronombre(ella, she).
traducir_pronombre(nosotros, we).
traducir_pronombre(nosotras, we).
traducir_pronombre(vosotros, you).
traducir_pronombre(vosotras, you).
traducir_pronombre(ellos, they).
traducir_pronombre(ellas, they).
traducir_pronombre(usted, you).
traducir_pronombre(ustedes, you).

% Traducción inversa
traducir_pronombre(i, yo).
traducir_pronombre(you, tú).
traducir_pronombre(he, él).
traducir_pronombre(she, ella).
traducir_pronombre(it, eso).
traducir_pronombre(we, nosotros).
traducir_pronombre(they, ellos).

% === INTERROGATIVOS ===
% interrogativo(idioma, palabra)

% Español:
interrogativo(esp, cómo).
interrogativo(esp, cuántos).
interrogativo(esp, cuál).
interrogativo(esp, dónde).
interrogativo(esp, quién).
interrogativo(esp, qué).

% Inglés:
interrogativo(eng, how).
interrogativo(eng, what).
interrogativo(eng, where).
interrogativo(eng, who).
interrogativo(eng, which).

% Traducción de interrogativos (español -> inglés)
traducir_interrogativo(cómo, how).
traducir_interrogativo(cuántos, how).
traducir_interrogativo(cuál, which).
traducir_interrogativo(dónde, where).
traducir_interrogativo(quién, who).
traducir_interrogativo(qué, what).

% Traducción inversa (inglés -> español)
traducir_interrogativo(how, cómo).
traducir_interrogativo(what, qué).
traducir_interrogativo(where, dónde).
traducir_interrogativo(who, quién).
traducir_interrogativo(which, cuál).

% === PREPOSICIONES ===
% preposicion(idioma, preposicion)

% Español:
preposicion(esp, en).
preposicion(esp, de).
preposicion(esp, con).
preposicion(esp, sobre).
preposicion(esp, para).
preposicion(esp, por).
preposicion(esp, sin).
preposicion(esp, bajo).
preposicion(esp, entre).
preposicion(esp, desde).
preposicion(esp, hasta).

% Inglés:
preposicion(eng, in).
preposicion(eng, of).
preposicion(eng, with).
preposicion(eng, on).
preposicion(eng, for).
preposicion(eng, by).
preposicion(eng, without).
preposicion(eng, under).
preposicion(eng, between).
preposicion(eng, from).
preposicion(eng, to).

% Traducción de preposiciones
traducir_preposicion(en, in).
traducir_preposicion(de, of).
traducir_preposicion(con, with).
traducir_preposicion(sobre, on).
traducir_preposicion(para, for).
traducir_preposicion(por, by).
traducir_preposicion(sin, without).
traducir_preposicion(bajo, under).
traducir_preposicion(entre, between).
traducir_preposicion(desde, from).
traducir_preposicion(hasta, to).

% Traducción inversa
traducir_preposicion(in, en).
traducir_preposicion(of, de).
traducir_preposicion(with, con).
traducir_preposicion(on, sobre).
traducir_preposicion(for, para).
traducir_preposicion(by, por).
traducir_preposicion(without, sin).
traducir_preposicion(under, bajo).
traducir_preposicion(between, entre).
traducir_preposicion(from, desde).
traducir_preposicion(to, hasta).

% === CONJUNCIONES ===
% conjuncion(idioma, conjuncion)

% Español:
conjuncion(esp, y).
conjuncion(esp, o).
conjuncion(esp, pero).
conjuncion(esp, porque).

% Inglés:
conjuncion(eng, and).
conjuncion(eng, or).
conjuncion(eng, but).
conjuncion(eng, because).

% Traducción de conjunciones
traducir_conjuncion(y, and).
traducir_conjuncion(o, or).
traducir_conjuncion(pero, but).
traducir_conjuncion(porque, because).

% Traducción inversa
traducir_conjuncion(and, y).
traducir_conjuncion(or, o).
traducir_conjuncion(but, pero).
traducir_conjuncion(because, porque).

% === NEGACIONES ===
% negacion(idioma, negacion)

% Español:
negacion(esp, no).

% Inglés:
negacion(eng, not).

% Traducción de negaciones
traducir_negacion(no, not).
traducir_negacion(not, no).