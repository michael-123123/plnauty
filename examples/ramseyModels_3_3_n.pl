ramseyModel(ramsey(3,3,3),map([[-1,A,B],[A,-1,C],[B,C,-1]]),[bool_arrays_lex([B],[C]),bool_arrays_lex([A],[B]),bool_array_or([A,B,C]),bool_array_or([-A,-B,-C])],[[-B,C],[-A,B],[A,B,C],[-A,-B,-C]]).

ramseyModel(ramsey(3,3,4),map([[-1,A,B,C],[A,-1,D,E],[B,D,-1,F],[C,E,F,-1]]),[bool_arrays_lex([B,C],[D,E]),bool_arrays_lex([A,B],[E,F]),bool_arrays_lex([A,E],[B,F]),bool_arrays_lex([B,D],[C,E]),bool_array_or([A,B,D]),bool_array_or([A,C,E]),bool_array_or([B,C,F]),bool_array_or([D,E,F]),bool_array_or([-A,-B,-D]),bool_array_or([-A,-C,-E]),bool_array_or([-B,-C,-F]),bool_array_or([-D,-E,-F])],[[D,-B],[-D,B,-G],[D,G],[-B,G],[-G,E,-C],[E,-A],[-E,A,-H],[E,H],[-A,H],[-H,F,-B],[B,-A],[-B,A,-I],[B,I],[-A,I],[-I,F,-E],[C,-B],[-C,B,-J],[C,J],[-B,J],[-J,E,-D],[A,B,D],[A,C,E],[B,C,F],[D,E,F],[-A,-B,-D],[-A,-C,-E],[-B,-C,-F],[-D,-E,-F]]).

ramseyModel(ramsey(3,3,5),map([[-1,A,B,C,D],[A,-1,E,F,G],[B,E,-1,H,I],[C,F,H,-1,J],[D,G,I,J,-1]]),[bool_arrays_lex([B,C,D],[E,F,G]),bool_arrays_lex([A,B,D],[F,H,J]),bool_arrays_lex([A,B,C],[G,I,J]),bool_arrays_lex([A,F,G],[B,H,I]),bool_arrays_lex([A,E,F],[D,I,J]),bool_arrays_lex([B,E,I],[C,F,J]),bool_arrays_lex([C,F,H],[D,G,I]),bool_array_or([A,B,E]),bool_array_or([A,C,F]),bool_array_or([A,D,G]),bool_array_or([B,C,H]),bool_array_or([B,D,I]),bool_array_or([C,D,J]),bool_array_or([E,F,H]),bool_array_or([E,G,I]),bool_array_or([F,G,J]),bool_array_or([H,I,J]),bool_array_or([-A,-B,-E]),bool_array_or([-A,-C,-F]),bool_array_or([-A,-D,-G]),bool_array_or([-B,-C,-H]),bool_array_or([-B,-D,-I]),bool_array_or([-C,-D,-J]),bool_array_or([-E,-F,-H]),bool_array_or([-E,-G,-I]),bool_array_or([-F,-G,-J]),bool_array_or([-H,-I,-J])],[[E,-B],[-E,B,-K],[E,K],[-B,K],[K,-L],[F,-C,-K],[-F,C,-L],[F,-K,L],[-C,-K,L],[-L,G,-D],[F,-A],[-F,A,-M],[F,M],[-A,M],[M,-N],[H,-B,-M],[-H,B,-N],[H,-M,N],[-B,-M,N],[-N,J,-D],[G,-A],[-G,A,-O],[G,O],[-A,O],[O,-P],[I,-B,-O],[-I,B,-P],[I,-O,P],[-B,-O,P],[-P,J,-C],[B,-A],[-B,A,-Q],[B,Q],[-A,Q],[Q,-R],[H,-F,-Q],[-H,F,-R],[H,-Q,R],[-F,-Q,R],[-R,I,-G],[D,-A],[-D,A,-S],[D,S],[-A,S],[S,-T],[I,-E,-S],[-I,E,-T],[I,-S,T],[-E,-S,T],[-T,J,-F],[C,-B],[-C,B,-U],[C,U],[-B,U],[U,-V],[F,-E,-U],[-F,E,-V],[F,-U,V],[-E,-U,V],[-V,J,-I],[D,-C],[-D,C,-W],[D,W],[-C,W],[W,-X],[G,-F,-W],[-G,F,-X],[G,-W,X],[-F,-W,X],[-X,I,-H],[A,B,E],[A,C,F],[A,D,G],[B,C,H],[B,D,I],[C,D,J],[E,F,H],[E,G,I],[F,G,J],[H,I,J],[-A,-B,-E],[-A,-C,-F],[-A,-D,-G],[-B,-C,-H],[-B,-D,-I],[-C,-D,-J],[-E,-F,-H],[-E,-G,-I],[-F,-G,-J],[-H,-I,-J]]).

ramseyModel(ramsey(3,3,6),map([[-1,A,B,C,D,E],[A,-1,F,G,H,I],[B,F,-1,J,K,L],[C,G,J,-1,M,N],[D,H,K,M,-1,O],[E,I,L,N,O,-1]]),[bool_arrays_lex([B,C,D,E],[F,G,H,I]),bool_arrays_lex([A,B,D,E],[G,J,M,N]),bool_arrays_lex([A,B,C,E],[H,K,M,O]),bool_arrays_lex([A,B,C,D],[I,L,N,O]),bool_arrays_lex([A,G,H,I],[B,J,K,L]),bool_arrays_lex([A,F,G,I],[D,K,M,O]),bool_arrays_lex([A,F,G,H],[E,L,N,O]),bool_arrays_lex([B,F,K,L],[C,G,M,N]),bool_arrays_lex([B,F,J,K],[E,I,N,O]),bool_arrays_lex([C,G,J,N],[D,H,K,O]),bool_arrays_lex([D,H,K,M],[E,I,L,N]),bool_array_or([A,B,F]),bool_array_or([A,C,G]),bool_array_or([A,D,H]),bool_array_or([A,E,I]),bool_array_or([B,C,J]),bool_array_or([B,D,K]),bool_array_or([B,E,L]),bool_array_or([C,D,M]),bool_array_or([C,E,N]),bool_array_or([D,E,O]),bool_array_or([F,G,J]),bool_array_or([F,H,K]),bool_array_or([F,I,L]),bool_array_or([G,H,M]),bool_array_or([G,I,N]),bool_array_or([H,I,O]),bool_array_or([J,K,M]),bool_array_or([J,L,N]),bool_array_or([K,L,O]),bool_array_or([M,N,O]),bool_array_or([-A,-B,-F]),bool_array_or([-A,-C,-G]),bool_array_or([-A,-D,-H]),bool_array_or([-A,-E,-I]),bool_array_or([-B,-C,-J]),bool_array_or([-B,-D,-K]),bool_array_or([-B,-E,-L]),bool_array_or([-C,-D,-M]),bool_array_or([-C,-E,-N]),bool_array_or([-D,-E,-O]),bool_array_or([-F,-G,-J]),bool_array_or([-F,-H,-K]),bool_array_or([-F,-I,-L]),bool_array_or([-G,-H,-M]),bool_array_or([-G,-I,-N]),bool_array_or([-H,-I,-O]),bool_array_or([-J,-K,-M]),bool_array_or([-J,-L,-N]),bool_array_or([-K,-L,-O]),bool_array_or([-M,-N,-O])],[[F,-B],[-F,B,-P],[F,P],[-B,P],[P,-Q],[G,-C,-P],[-G,C,-Q],[G,-P,Q],[-C,-P,Q],[Q,-R],[H,-D,-Q],[-H,D,-R],[H,-Q,R],[-D,-Q,R],[-R,I,-E],[G,-A],[-G,A,-S],[G,S],[-A,S],[S,-T],[J,-B,-S],[-J,B,-T],[J,-S,T],[-B,-S,T],[T,-U],[M,-D,-T],[-M,D,-U],[M,-T,U],[-D,-T,U],[-U,N,-E],[H,-A],[-H,A,-V],[H,V],[-A,V],[V,-W],[K,-B,-V],[-K,B,-W],[K,-V,W],[-B,-V,W],[W,-X],[M,-C,-W],[-M,C,-X],[M,-W,X],[-C,-W,X],[-X,O,-E],[I,-A],[-I,A,-Y],[I,Y],[-A,Y],[Y,-Z],[L,-B,-Y],[-L,B,-Z],[L,-Y,Z],[-B,-Y,Z],[Z,-A1],[N,-C,-Z],[-N,C,-A1],[N,-Z,A1],[-C,-Z,A1],[-A1,O,-D],[B,-A],[-B,A,-B1],[B,B1],[-A,B1],[B1,-C1],[J,-G,-B1],[-J,G,-C1],[J,-B1,C1],[-G,-B1,C1],[C1,-D1],[K,-H,-C1],[-K,H,-D1],[K,-C1,D1],[-H,-C1,D1],[-D1,L,-I],[D,-A],[-D,A,-E1],[D,E1],[-A,E1],[E1,-F1],[K,-F,-E1],[-K,F,-F1],[K,-E1,F1],[-F,-E1,F1],[F1,-G1],[M,-G,-F1],[-M,G,-G1],[M,-F1,G1],[-G,-F1,G1],[-G1,O,-I],[E,-A],[-E,A,-H1],[E,H1],[-A,H1],[H1,-I1],[L,-F,-H1],[-L,F,-I1],[L,-H1,I1],[-F,-H1,I1],[I1,-J1],[N,-G,-I1],[-N,G,-J1],[N,-I1,J1],[-G,-I1,J1],[-J1,O,-H],[C,-B],[-C,B,-K1],[C,K1],[-B,K1],[K1,-L1],[G,-F,-K1],[-G,F,-L1],[G,-K1,L1],[-F,-K1,L1],[L1,-M1],[M,-K,-L1],[-M,K,-M1],[M,-L1,M1],[-K,-L1,M1],[-M1,N,-L],[E,-B],[-E,B,-N1],[E,N1],[-B,N1],[N1,-O1],[I,-F,-N1],[-I,F,-O1],[I,-N1,O1],[-F,-N1,O1],[O1,-P1],[N,-J,-O1],[-N,J,-P1],[N,-O1,P1],[-J,-O1,P1],[-P1,O,-K],[D,-C],[-D,C,-Q1],[D,Q1],[-C,Q1],[Q1,-R1],[H,-G,-Q1],[-H,G,-R1],[H,-Q1,R1],[-G,-Q1,R1],[R1,-S1],[K,-J,-R1],[-K,J,-S1],[K,-R1,S1],[-J,-R1,S1],[-S1,O,-N],[E,-D],[-E,D,-T1],[E,T1],[-D,T1],[T1,-U1],[I,-H,-T1],[-I,H,-U1],[I,-T1,U1],[-H,-T1,U1],[U1,-V1],[L,-K,-U1],[-L,K,-V1],[L,-U1,V1],[-K,-U1,V1],[-V1,N,-M],[A,B,F],[A,C,G],[A,D,H],[A,E,I],[B,C,J],[B,D,K],[B,E,L],[C,D,M],[C,E,N],[D,E,O],[F,G,J],[F,H,K],[F,I,L],[G,H,M],[G,I,N],[H,I,O],[J,K,M],[J,L,N],[K,L,O],[M,N,O],[-A,-B,-F],[-A,-C,-G],[-A,-D,-H],[-A,-E,-I],[-B,-C,-J],[-B,-D,-K],[-B,-E,-L],[-C,-D,-M],[-C,-E,-N],[-D,-E,-O],[-F,-G,-J],[-F,-H,-K],[-F,-I,-L],[-G,-H,-M],[-G,-I,-N],[-H,-I,-O],[-J,-K,-M],[-J,-L,-N],[-K,-L,-O],[-M,-N,-O]]).

