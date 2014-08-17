
let enc_latin1 = "latin1"
let enc_utf8 = "utf-8"
let enc_koi8r = "koi8-r"
let enc_cp1251 = "cp1251"

let pairs = Hashtbl.create 300

let stat t =
	let t = Text.lower t in
	let score = ref 0 in
	for i=0 to (Text.length t)-2 do
		let pair = Text.sub t i 2 in
		try
			score := !score + (Hashtbl.find pairs pair)
		with
			| _ -> ()
	done;
	!score

let detect_charset t =
	let open Encoding in
	try
		let t = recode_string ~src:enc_utf8 ~dst:enc_latin1 t in

		let st1 = recode_string ~src:enc_koi8r ~dst:enc_utf8 t |> stat in
		let st2 = recode_string ~src:enc_cp1251 ~dst:enc_utf8 t |> stat in

		if st1 = st2 then None
		else if st1 > st2 then Some enc_koi8r else Some enc_cp1251
	with
		| _ -> None

let () =
	Hashtbl.add pairs "пп" 151;
	Hashtbl.add pairs "вя" 153;
	Hashtbl.add pairs "кс" 153;
	Hashtbl.add pairs "мя" 153;
	Hashtbl.add pairs "тя" 153;
	Hashtbl.add pairs "ащ" 156;
	Hashtbl.add pairs "йн" 159;
	Hashtbl.add pairs "вк" 160;
	Hashtbl.add pairs "шо" 161;
	Hashtbl.add pairs "ян" 164;
	Hashtbl.add pairs "фи" 168;
	Hashtbl.add pairs "пу" 169;
	Hashtbl.add pairs "зр" 171;
	Hashtbl.add pairs "гу" 173;
	Hashtbl.add pairs "ью" 175;
	Hashtbl.add pairs "ям" 175;
	Hashtbl.add pairs "дк" 181;
	Hashtbl.add pairs "йс" 181;
	Hashtbl.add pairs "гл" 182;
	Hashtbl.add pairs "ша" 182;
	Hashtbl.add pairs "ды" 183;
	Hashtbl.add pairs "ср" 183;
	Hashtbl.add pairs "тд" 185;
	Hashtbl.add pairs "мм" 188;
	Hashtbl.add pairs "яд" 188;
	Hashtbl.add pairs "уа" 193;
	Hashtbl.add pairs "вт" 194;
	Hashtbl.add pairs "ув" 194;
	Hashtbl.add pairs "ха" 194;
	Hashtbl.add pairs "аи" 196;
	Hashtbl.add pairs "нс" 198;
	Hashtbl.add pairs "яю" 198;
	Hashtbl.add pairs "кл" 199;
	Hashtbl.add pairs "ыл" 200;
	Hashtbl.add pairs "зи" 201;
	Hashtbl.add pairs "ех" 202;
	Hashtbl.add pairs "нф" 206;
	Hashtbl.add pairs "пл" 215;
	Hashtbl.add pairs "см" 215;
	Hashtbl.add pairs "ыб" 218;
	Hashtbl.add pairs "еш" 220;
	Hashtbl.add pairs "дв" 231;
	Hashtbl.add pairs "бх" 232;
	Hashtbl.add pairs "ущ" 232;
	Hashtbl.add pairs "рв" 233;
	Hashtbl.add pairs "иб" 235;
	Hashtbl.add pairs "зд" 236;
	Hashtbl.add pairs "ук" 236;
	Hashtbl.add pairs "лн" 237;
	Hashtbl.add pairs "би" 238;
	Hashtbl.add pairs "яз" 238;
	Hashtbl.add pairs "зл" 241;
	Hashtbl.add pairs "ьз" 242;
	Hashtbl.add pairs "еж" 246;
	Hashtbl.add pairs "оя" 247;
	Hashtbl.add pairs "ум" 250;
	Hashtbl.add pairs "вр" 251;
	Hashtbl.add pairs "зм" 251;
	Hashtbl.add pairs "аг" 256;
	Hashtbl.add pairs "зв" 258;
	Hashtbl.add pairs "иц" 259;
	Hashtbl.add pairs "оо" 261;
	Hashtbl.add pairs "ид" 268;
	Hashtbl.add pairs "нь" 268;
	Hashtbl.add pairs "ъе" 271;
	Hashtbl.add pairs "ип" 276;
	Hashtbl.add pairs "бъ" 287;
	Hashtbl.add pairs "дс" 288;
	Hashtbl.add pairs "уе" 299;
	Hashtbl.add pairs "рж" 300;
	Hashtbl.add pairs "ыс" 300;
	Hashtbl.add pairs "ря" 308;
	Hashtbl.add pairs "ур" 308;
	Hashtbl.add pairs "ющ" 309;
	Hashtbl.add pairs "ул" 310;
	Hashtbl.add pairs "нц" 311;
	Hashtbl.add pairs "бщ" 313;
	Hashtbl.add pairs "ои" 321;
	Hashtbl.add pairs "ео" 335;
	Hashtbl.add pairs "ев" 341;
	Hashtbl.add pairs "аш" 344;
	Hashtbl.add pairs "зы" 347;
	Hashtbl.add pairs "пи" 357;
	Hashtbl.add pairs "ву" 358;
	Hashtbl.add pairs "па" 358;
	Hashtbl.add pairs "зо" 365;
	Hashtbl.add pairs "щи" 368;
	Hashtbl.add pairs "ез" 369;
	Hashtbl.add pairs "ах" 373;
	Hashtbl.add pairs "бу" 373;
	Hashtbl.add pairs "ьс" 374;
	Hashtbl.add pairs "еп" 377;
	Hashtbl.add pairs "иа" 385;
	Hashtbl.add pairs "ош" 389;
	Hashtbl.add pairs "еб" 393;
	Hashtbl.add pairs "ап" 394;
	Hashtbl.add pairs "уг" 394;
	Hashtbl.add pairs "зу" 399;
	Hashtbl.add pairs "тк" 404;
	Hashtbl.add pairs "яе" 411;
	Hashtbl.add pairs "яв" 415;
	Hashtbl.add pairs "бл" 420;
	Hashtbl.add pairs "уж" 421;
	Hashtbl.add pairs "ьш" 423;
	Hashtbl.add pairs "жи" 424;
	Hashtbl.add pairs "ут" 430;
	Hashtbl.add pairs "лю" 436;
	Hashtbl.add pairs "сы" 441;
	Hashtbl.add pairs "ию" 455;
	Hashtbl.add pairs "ус" 456;
	Hashtbl.add pairs "др" 459;
	Hashtbl.add pairs "уп" 460;
	Hashtbl.add pairs "сь" 461;
	Hashtbl.add pairs "жа" 467;
	Hashtbl.add pairs "га" 472;
	Hashtbl.add pairs "бр" 473;
	Hashtbl.add pairs "ну" 480;
	Hashtbl.add pairs "ац" 481;
	Hashtbl.add pairs "бе" 481;
	Hashtbl.add pairs "еч" 488;
	Hashtbl.add pairs "ду" 490;
	Hashtbl.add pairs "жд" 494;
	Hashtbl.add pairs "св" 495;
	Hashtbl.add pairs "гд" 497;
	Hashtbl.add pairs "гр" 505;
	Hashtbl.add pairs "су" 518;
	Hashtbl.add pairs "мн" 524;
	Hashtbl.add pairs "сн" 544;
	Hashtbl.add pairs "ыт" 545;
	Hashtbl.add pairs "ад" 546;
	Hashtbl.add pairs "ку" 548;
	Hashtbl.add pairs "фо" 552;
	Hashtbl.add pairs "ар" 560;
	Hashtbl.add pairs "кр" 560;
	Hashtbl.add pairs "ок" 562;
	Hashtbl.add pairs "ши" 569;
	Hashtbl.add pairs "рм" 582;
	Hashtbl.add pairs "дл" 583;
	Hashtbl.add pairs "ьк" 585;
	Hashtbl.add pairs "ьт" 587;
	Hashtbl.add pairs "оз" 594;
	Hashtbl.add pairs "уд" 615;
	Hashtbl.add pairs "ей" 634;
	Hashtbl.add pairs "ыв" 639;
	Hashtbl.add pairs "оч" 645;
	Hashtbl.add pairs "це" 646;
	Hashtbl.add pairs "ик" 648;
	Hashtbl.add pairs "ир" 653;
	Hashtbl.add pairs "ры" 653;
	Hashtbl.add pairs "ый" 654;
	Hashtbl.add pairs "ач" 660;
	Hashtbl.add pairs "аю" 673;
	Hashtbl.add pairs "ым" 677;
	Hashtbl.add pairs "ее" 698;
	Hashtbl.add pairs "аж" 712;
	Hashtbl.add pairs "ще" 713;
	Hashtbl.add pairs "ня" 719;
	Hashtbl.add pairs "си" 725;
	Hashtbl.add pairs "рн" 740;
	Hashtbl.add pairs "ио" 746;
	Hashtbl.add pairs "ая" 760;
	Hashtbl.add pairs "вс" 769;
	Hashtbl.add pairs "ий" 776;
	Hashtbl.add pairs "дн" 778;
	Hashtbl.add pairs "вн" 782;
	Hashtbl.add pairs "ии" 784;
	Hashtbl.add pairs "лу" 785;
	Hashtbl.add pairs "чн" 785;
	Hashtbl.add pairs "пе" 800;
	Hashtbl.add pairs "аб" 806;
	Hashtbl.add pairs "ую" 809;
	Hashtbl.add pairs "ше" 817;
	Hashtbl.add pairs "ют" 823;
	Hashtbl.add pairs "жн" 825;
	Hashtbl.add pairs "му" 829;
	Hashtbl.add pairs "ги" 840;
	Hashtbl.add pairs "тн" 852;
	Hashtbl.add pairs "кт" 856;
	Hashtbl.add pairs "ек" 857;
	Hashtbl.add pairs "зн" 871;
	Hashtbl.add pairs "ту" 872;
	Hashtbl.add pairs "нд" 900;
	Hashtbl.add pairs "мы" 913;
	Hashtbl.add pairs "ла" 932;
	Hashtbl.add pairs "ят" 932;
	Hashtbl.add pairs "ых" 943;
	Hashtbl.add pairs "оц" 949;
	Hashtbl.add pairs "уч" 960;
	Hashtbl.add pairs "вл" 969;
	Hashtbl.add pairs "ми" 987;
	Hashtbl.add pairs "ег" 1001;
	Hashtbl.add pairs "ые" 1002;
	Hashtbl.add pairs "ты" 1011;
	Hashtbl.add pairs "са" 1023;
	Hashtbl.add pairs "сс" 1024;
	Hashtbl.add pairs "ля" 1030;
	Hashtbl.add pairs "хо" 1034;
	Hashtbl.add pairs "эт" 1044;
	Hashtbl.add pairs "из" 1050;
	Hashtbl.add pairs "чи" 1051;
	Hashtbl.add pairs "бы" 1069;
	Hashtbl.add pairs "нк" 1072;
	Hashtbl.add pairs "их" 1075;
	Hashtbl.add pairs "ке" 1091;
	Hashtbl.add pairs "чт" 1102;
	Hashtbl.add pairs "ас" 1140;
	Hashtbl.add pairs "ма" 1169;
	Hashtbl.add pairs "ам" 1171;
	Hashtbl.add pairs "ди" 1180;
	Hashtbl.add pairs "ки" 1188;
	Hashtbl.add pairs "ру" 1212;
	Hashtbl.add pairs "се" 1236;
	Hashtbl.add pairs "ви" 1249;
	Hashtbl.add pairs "ча" 1278;
	Hashtbl.add pairs "ое" 1292;
	Hashtbl.add pairs "ав" 1401;
	Hashtbl.add pairs "вы" 1462;
	Hashtbl.add pairs "ож" 1480;
	Hashtbl.add pairs "тр" 1506;
	Hashtbl.add pairs "ил" 1535;
	Hashtbl.add pairs "тс" 1539;
	Hashtbl.add pairs "ив" 1555;
	Hashtbl.add pairs "аз" 1556;
	Hashtbl.add pairs "нт" 1628;
	Hashtbl.add pairs "бо" 1634;
	Hashtbl.add pairs "ьн" 1634;
	Hashtbl.add pairs "же" 1655;
	Hashtbl.add pairs "ци" 1666;
	Hashtbl.add pairs "ич" 1702;
	Hashtbl.add pairs "ой" 1722;
	Hashtbl.add pairs "сп" 1740;
	Hashtbl.add pairs "за" 1749;
	Hashtbl.add pairs "ск" 1766;
	Hashtbl.add pairs "да" 1795;
	Hashtbl.add pairs "ае" 1807;
	Hashtbl.add pairs "ин" 1811;
	Hashtbl.add pairs "ся" 1884;
	Hashtbl.add pairs "до" 1919;
	Hashtbl.add pairs "ис" 1936;
	Hashtbl.add pairs "нн" 1959;
	Hashtbl.add pairs "мо" 2076;
	Hashtbl.add pairs "ме" 2096;
	Hashtbl.add pairs "ие" 2101;
	Hashtbl.add pairs "им" 2138;
	Hashtbl.add pairs "ал" 2151;
	Hashtbl.add pairs "ем" 2261;
	Hashtbl.add pairs "тв" 2289;
	Hashtbl.add pairs "сл" 2296;
	Hashtbl.add pairs "че" 2313;
	Hashtbl.add pairs "ри" 2332;
	Hashtbl.add pairs "ит" 2344;
	Hashtbl.add pairs "од" 2360;
	Hashtbl.add pairs "об" 2368;
	Hashtbl.add pairs "ом" 2397;
	Hashtbl.add pairs "ел" 2402;
	Hashtbl.add pairs "ак" 2417;
	Hashtbl.add pairs "ор" 2433;
	Hashtbl.add pairs "ны" 2458;
	Hashtbl.add pairs "ия" 2481;
	Hashtbl.add pairs "со" 2532;
	Hashtbl.add pairs "ве" 2615;
	Hashtbl.add pairs "ло" 2682;
	Hashtbl.add pairs "он" 2712;
	Hashtbl.add pairs "ед" 2743;
	Hashtbl.add pairs "ка" 2791;
	Hashtbl.add pairs "оп" 2823;
	Hashtbl.add pairs "ле" 2835;
	Hashtbl.add pairs "го" 2849;
	Hashtbl.add pairs "ер" 2862;
	Hashtbl.add pairs "ат" 2900;
	Hashtbl.add pairs "ог" 3140;
	Hashtbl.add pairs "де" 3141;
	Hashtbl.add pairs "та" 3197;
	Hashtbl.add pairs "ли" 3214;
	Hashtbl.add pairs "ть" 3250;
	Hashtbl.add pairs "ти" 3330;
	Hashtbl.add pairs "ва" 3464;
	Hashtbl.add pairs "ол" 3594;
	Hashtbl.add pairs "ль" 3655;
	Hashtbl.add pairs "ан" 3770;
	Hashtbl.add pairs "ес" 3797;
	Hashtbl.add pairs "ко" 3800;
	Hashtbl.add pairs "не" 3850;
	Hashtbl.add pairs "те" 3850;
	Hashtbl.add pairs "от" 4159;
	Hashtbl.add pairs "ре" 4225;
	Hashtbl.add pairs "на" 4268;
	Hashtbl.add pairs "ра" 4301;
	Hashtbl.add pairs "во" 4353;
	Hashtbl.add pairs "ро" 4806;
	Hashtbl.add pairs "ов" 5007;
	Hashtbl.add pairs "по" 5290;
	Hashtbl.add pairs "ос" 5435;
	Hashtbl.add pairs "ет" 5445;
	Hashtbl.add pairs "ст" 5744;
	Hashtbl.add pairs "ни" 5851;
	Hashtbl.add pairs "то" 5912;
	Hashtbl.add pairs "ен" 6071;
	Hashtbl.add pairs "пр" 6480;
	Hashtbl.add pairs "но" 6599