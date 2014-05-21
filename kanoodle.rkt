(module a10 (lib "plt-pretty-big-text.ss" "lang")
  
  (require "a10.rkt")
  
  ;; These examples come from the puzzle game Kanoodle
  ;; (see, for example, http://www.youtube.com/watch?v=MfkBntdMUYo).
  ;; They are taken from Levels 2, 3, 5 and 6 (which correspond to
  ;; the number of pieces that must be filled in).
  
  ;; The complete list Kanoodle pieces
  
  (define k/A (strlist->grid '("AAA" "A..")))
  (define k/B (strlist->grid '("BBB" "BB.")))
  (define k/C (strlist->grid '("CCCC" "C...")))
  (define k/D (strlist->grid '("DDDD" ".D..")))
  (define k/E (strlist->grid '("EEE." "..EE")))
  (define k/F (strlist->grid '("FF" "F.")))
  (define k/G (strlist->grid '("GGG" "G.." "G..")))
  (define k/H (strlist->grid '("..H" ".HH" "HH.")))
  (define k/I (strlist->grid '("I.I" "III")))
  (define k/J (strlist->grid '("JJJJ")))
  (define k/K (strlist->grid '("KK" "KK")))
  (define k/L (strlist->grid '(".L." "LLL" ".L.")))
  
  ;; Sample grids and piece lists.
  
  (define kanoodle-2/1-g
    (strlist->grid
     '("HHBBBCCCCKK" "DHHBBCAAAKK" "DDHEELIIA.." "DEEELLLI..." "DJJJJLII...")))
  (define kanoodle-2/1-p (list k/F k/G))
  
  (define kanoodle-2/2-g
    (strlist->grid
     '("JJJJHHDDDDF" "CCIILHHDEFF" "CGILLLHEE.." "CGIILBBE..." "CGGGBBBE...")))
  (define kanoodle-2/2-p (list k/A k/K))
  
  (define kanoodle-2/3-g
    (strlist->grid
     '("EEGGGDDDDKK" "JEEEGCDHHKK" "JIILGCHH..." "JILLLCHF..." "JIILCCFF...")))
  (define kanoodle-2/3-p (list k/A k/B))
  
  (define kanoodle-2/4-g
    (strlist->grid
     '("BBDDDDHHAAA" "BBBFDHHEEEA" "IILFFHEE..." "ILLLCCCC..." "IILJJJJC...")))
  (define kanoodle-2/4-p (list k/G k/K))
  
  
  
  (define kanoodle-3/1-g
    (strlist->grid
     '("JIILFFKK..." "JILLLFKK..." "JIILEEHA..." "JDEEEHHA..." "DDDDHHAA...")))
  (define kanoodle-3/1-p (list k/B k/C k/G))
  
  (define kanoodle-3/2-g
    (strlist->grid
     '("JJJJDDDD..." "GGGLHHDC..." "GALLLHHC..." "GAFLKKHC..." "AAFFKKCC...")))
  (define kanoodle-3/2-p (list k/B k/E k/I))
  
  (define kanoodle-3/3-g
    (strlist->grid
     '("FFHIIIKK..." "FHHILIKK..." "HHDLLLAA..." "DDDDLBBA..." "JJJJBBBA...")))
  (define kanoodle-3/3-p (list k/C k/E k/G))
  
  (define kanoodle-3/4-g
    (strlist->grid 
     '("KKEEEAAC..." "KKHHEEAC..." "GHHLFFAC..." "GHLLLFCC..." "GGGLJJJJ...")))
  (define kanoodle-3/4-p (list k/B k/D k/I))
  
  
  
  (define kanoodle-5/1-g
    (strlist->grid 
     '("FFDDDDEEE.." "FKKDLEE...." "GKKLLL....." "GAAAL......" "GGGA.......")))
  (define kanoodle-5/1-p (list k/B k/C k/H k/I k/J))
  
  (define kanoodle-5/2-g
    (strlist->grid
     '("FDDDDE....." "FFDCCE....." "GKKCEE....." "GKKCE......" "GGGCJJJJ...")))
  (define kanoodle-5/2-p (list k/A k/B k/H k/I k/L))
  
  (define kanoodle-5/3-g
    (strlist->grid
     '("FFKKBB....." "FCKKBBB...." "GCCCCL....." "GEEELLL...." "GGGEEL.....")))
  (define kanoodle-5/3-p (list k/A k/D k/H k/I k/J))
  
  (define kanoodle-5/4-g
    (strlist->grid
     '("FJJJJEE...." "FFHEEE....." "KKHHII....." "KKDHHI....." "DDDDII.....")))
  (define kanoodle-5/4-p (list k/A k/B k/C k/G k/L))
  
  
  (define kanoodle-6/1-g
    (strlist->grid
     '("IIIDDDD...." "IEIFD......" "EEFF......." "EAAA......." "EAJJJJ.....")))
  (define kanoodle-6/1-p (list k/B k/C k/G k/H k/K k/L))
  
  (define kanoodle-6/2-g
    (strlist->grid
     '("IIIAD......" "IBIADD....." "BBAAD......" "BBEED......" "EEEJJJJ....")))
  (define kanoodle-6/2-p (list k/C k/F k/G k/H k/K k/L))
  
  (define kanoodle-6/3-g
    (strlist->grid
     '("IIJJJJ....." "IDDDD......" "IIADG......" "AAACG......" "CCCCGGG....")))
  (define kanoodle-6/3-p (list k/B k/E k/F k/H k/K k/L))
  
  (define kanoodle-6/4-g
    (strlist->grid 
     '("IIDDDD....." "IEEED......" "IIFEE......" "AFFKK......" "AAAKK......")))
  (define kanoodle-6/4-p (list k/B k/C k/G k/H k/J k/L)) 
  
  
  (provide 
   kanoodle-2/1-g
   kanoodle-2/1-p
   kanoodle-2/2-g
   kanoodle-2/2-p
   kanoodle-2/3-g
   kanoodle-2/3-p
   kanoodle-2/4-g
   kanoodle-2/4-p
   
   kanoodle-3/1-g
   kanoodle-3/1-p
   kanoodle-3/2-g
   kanoodle-3/2-p
   kanoodle-3/3-g
   kanoodle-3/3-p
   kanoodle-3/4-g
   kanoodle-3/4-p
   
   kanoodle-5/1-g
   kanoodle-5/1-p
   kanoodle-5/2-g
   kanoodle-5/2-p
   kanoodle-5/3-g
   kanoodle-5/3-p
   kanoodle-5/4-g
   kanoodle-5/4-p
   
   kanoodle-6/1-g
   kanoodle-6/1-p
   kanoodle-6/2-g
   kanoodle-6/2-p
   kanoodle-6/3-g
   kanoodle-6/3-p
   kanoodle-6/4-g
   kanoodle-6/4-p))