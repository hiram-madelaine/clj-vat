(ns clj-vat.algo-test
  #+cljs  (:require-macros [cemerick.cljs.test
                    :refer (is are deftest with-test run-tests testing test-var)])
  #+cljs (:require [cemerick.cljs.test :as t]
                   [clj-vat.algo :as a])
  #+clj (:use clojure.test)
  #+clj (:require [clj-vat.algo :as a]))


(deftest test-->digits
  (are [i r] (= r (a/->digits i))
       "12345" [1 2 3 4 5]
       ["1" "2" "3" "4"] [1 2 3 4]
       12345 [1 2 3 4 5]))


(deftest test-+>9
  (are [i r] (= (a/+>9 i) r)
       12 3
       9 9
       111 3
       10 1
       11 2
       0 0))


(deftest test-luhn
  (are [i r] (= (a/luhn i) r)
       "5136160214196774" true
       "5136160214196773" false))

(deftest test->num
  (are [i r] (= r (a/->num i))
       [1 2 3 4 ] 1234))

(deftest test-check-vat-fr-KO
  (are [i] (false? (a/check-ident i))
           "FR00325422482"))


(deftest test-check-vat-fr
  (are [i] (true? (a/check-vat-fr i))
           "00325422483"
           "00504137126"
           "01308148691"
           "02410043764"
           "03533645594"
           "05327925699"
           "05332944867"
           "05999990005"
           "09493241798"
           "10615920188"
           "11055501977"
           "11349352419"
           "12334502366"
           "12382577021"
           "12402385876"
           "12441639739"
           "12488632068"
           "13379608110"
           "15312631195"
           "17409032059"
           "17440389831"
           "18306369943"
           "18317328227"
           "18432626307"
           "20552140816"
           "22318546322"
           "22501687560"
           "22577380249"
           "22681750212"
           "25384801882"
           "25578801300"
           "26311799456"
           "26491219192"
           "26494929151"
           "27367801404"
           "28403857436"
           "29552088536"
           "31947251922"
           "32392677282"
           "36438324675"
           "37386220123"
           "37714501491"
           "38315907022"
           "38348352261"
           "38413058439"
           "38430097071"
           "38785620501"
           "39309634582"
           "39440270981"
           "40524399631"
           "41339045403"
           "41387565385"
           "41560502320"
           "42303993257"
           "43750542037"
           "44328918313"
           "44388481939"
           "45334835766"
           "46542060132"
           "47316474766"
           "47751918921"
           "48385406814"
           "49410252944"
           "49410323463"
           "49450767322"
           "51671920080"
           "52453093965"
           "52576480099"
           "53323181057"
           "53412603417"
           "54440085231"
           "55352496624"
           "56344752209"
           "57349033951"
           "57414817023"
           "58389479718"
           "58449286426"
           "59354500225"
           "59384822037"
           "59432429249"
           "60338216609"
           "60379011414"
           "60485314199"
           "60504648627"
           "60542084322"
           "60801543414"
           "61418574869"
           "62333286714"
           "63351577002"
           "66046850145"
           "68380313189"
           "68421343898"
           "68430360727"
           "68622001881"
           "69430456046"
           "69644200784"
           "71344122296"
           "72562008029"
           "75477632095"
           "76382290278"
           "76410255087"
           "78418114351"
           "79343464186"
           "82400519005"
           "82542007182"
           "83542050315"
           "84301125175"
           "85753182294"
           "85877280602"
           "87784173536"
           "88626250229"
           "89307049387"
           "89451190029"
           "89753948272"
           "91602002461"
           "92095780284"
           "92443201595"
           "93442769428"
           "94315030387"
           "94926450099"
           "95441297777"
           "95716220512"
           "96343149944"
           "96527512800"
           "96532768066"))



(deftest test-check-vat-gb-KO
  (are [i] (false? (a/check-ident i))
           "GB100146191"))


(deftest test-check-vat-gb
  (are [i] (true? (a/check-vat-gb i))
           "100146190"
           "100946989"
           "101474653"
           "103471944"
           "103590836"
           "105289914"
           "106125319"
           "106824188"
           "112341372"
           "112452654"
           "116440346"
           "116755802"
           "116910921"
           "119929241"
           "124808476"
           "125073834"
           "125342442"
           "126846944"
           "127747058"
           "131712157"
           "136374368"
           "137672493"
           "138025976"
           "140165745"
           "140840831"
           "142313749"
           "144137825"
           "144553711"
           "150045555"
           "154578582"
           "158291978"
           "160321161"
           "163597044"
           "168825915"
           "172351381"
           "173047125"
           "179387449"
           "182880774"
           "191044775"
           "197531237"
           "198187115"
           "202329313"
           "202429210"
           "213221231"
           "226556555"
           "238406267"
           "243208980"
           "246022296"
           "247103682"
           "254347459"
           "255073076"
           "275913139"
           "281355458"
           "288638008"
           "289234622"
           "303869943"
           "304766951"
           "307023112"
           "308302200"
           "308816452"
           "311057215"
           "316694833"
           "333851166"
           "339072747"
           "344407665"
           "347154553"
           "347597116"
           "349917606"
           "356527143"
           "378038429"
           "378097310"
           "397759080"
           "404065883"
           "412144601"
           "412672765"
           "414857050"
           "427335750"
           "429151752"
           "435000112"
           "439806813"
           "441831073"
           "445004484"
           "452052481"
           "461689319"
           "464275435"
           "468192123"
           "478080229"
           "484658693"
           "507642158"
           "516716936"
           "516884909"
           "518441844"
           "530215790"
           "537471726"
           "549798270"
           "553124860"
           "555054253"
           "557285804"
           "569869162"
           "570093452"
           "571258535"
           "581189224"
           "581198517"
           "581379120"
           "582341248"
           "593395888"
           "594261226"
           "594277693"
           "603871841"
           "607131476"
           "610809954"
           "611635076"
           "613731169"
           "619327237"
           "623008384"
           "624850146"
           "625512459"
           "631570164"
           "637035056"
           "641614164"
           "652253354"
           "662043655"
           "666322529"
           "674663206"
           "675597082"
           "679736957"
           "683254030"
           "683741214"
           "683867574"
           "689476165"
           "693195792"
           "694264601"
           "702719158"
           "703265267"
           "706725436"
           "712061386"
           "713804551"
           "714293150"
           "724283637"
           "724868502"
           "728520043"
           "733763031"
           "750044467"
           "750347737"
           "754173037"
           "759894254"
           "761236932"
           "763160441"
           "765365012"
           "768461392"
           "769784844"
           "770046839"
           "770817808"
           "771422931"
           "775247012"
           "778488846"
           "782333230"
           "783419405"
           "788603190"
           "788715077"
           "792409995"
           "794661973"
           "795095973"
           "795834568"
           "798573748"
           "803601569"
           "806607141"
           "810727157"
           "811094951"
           "816744906"
           "817032259"
           "825439027"
           "825823911"
           "829703900"
           "829890083"
           "830143667"
           "830865716"
           "836965674"
           "840722445"
           "846390994"
           "846528401"
           "847292106"
           "848243215"
           "858321217"
           "859484564"
           "861153244"
           "862819984"
           "862833505"
           "863009827"
           "864440910"
           "864721704"
           "866897052"
           "876152993"
           "880989655"
           "884493180"
           "892207710"
           "892741001"
           "899739918"
           "901232480"
           "901407467"
           "908691592"
           "912333361"
           "918198491"
           "918515023"
           "927114437"
           "927534115"
           "927577383"
           "929539969"
           "930303861"
           "930897591"
           "932869781"
           "933393415"
           "935273906"
           "938240223"
           "938827767"
           "945371312"
           "947205321"
           "947223711"
           "970899459"
           "971019624"
           "971082617"
           "973763084"
           "974666662"
           "974957354"
           "979190565"
           "979346459"
           "979389434"
           "981468383"
           "983190496"
           "984902088"
           "985134203"
           "986200020"
           "987966426"
           "996868620"
           "998718045"))



(deftest test-check-vat-dk-KO
  (are [i] (false? (a/check-ident i))
           "DK13585629"))

(deftest test-check-vat-dk
  (are [i](true? (a/check-vat-dk i))
       "13585628"
       "29425434"
       "67679628"
       "30861310"
       "56423818"
       "26366313"
       "25922921"
       "17026518"
       "27998895"
       "10664446"
       "27224849"
       "25586840"
       "15027800"
       "29425434"
       "81745528"))


(deftest test-check-vat-de-KO
  (are [i] (false? (a/check-ident i))
           "DE116213912"))

(deftest test-check-vat-de
  (are [i] (true? (a/check-vat-de i))
           "116213911"
           "117658764"
           "117662628"
           "121967016"
           "123045750"
           "125081842"
           "126957135"
           "131934331"
           "136695976"
           "139233160"
           "146130661"
           "148418567"
           "148418567"
           "149320417"
           "157336064"
           "157796279"
           "170179642"
           "183677859"
           "189915799"
           "206169012"
           "209867508"
           "220716351"
           "236097644"
           "242080537"
           "254117387"
           "254176356"
           "255505922"
           "256092693"
           "260007172"
           "261263875"
           "261811370"
           "267301179"
           "267844793"
           "273414958"
           "273967855"
           "279201460"
           "281604179"
           "284928405"
           "292143324"
           "811152493"
           "811152493"
           "811152493"
           "811152493"
           "811152493"
           "811423486"
           "811698404"
           "812139660"
           "812227901"
           "812463252"
           "812761885"
           "812803588"
           "813337219"
           "813632841"
           "813821430"
           "813851493"))


(deftest test-check-vat-pl-KO
  (are [i] (false? (a/check-ident i))
           "PL1010003755"))

 (deftest test-check-vat-pl
   (are [i] (true? (a/check-vat-pl i))
            "1010003756"
            "1130017933"
            "1130039461"
            "1182081972"
            "1230007271"
            "5210086737"
            "5210089960"
            "5213405781"
            "5220103934"
            "5240401310"
            "5242388571"
            "5252109503"
            "5261013323"
            "5270022391"
            "5270103824"
            "5270104717"
            "5841031823"
            "5842394781"
            "5851356886"
            "5871283050"
            "5890011743"
            "5911003253"
            "6060069352"
            "6151666800"
            "6180017056"
            "6272494255"
            "6460000312"
            "6621004747"
            "6681941935"
            "7250013591"
            "7251959614"
            "7610002225"
            "7761002967"
            "7773224366"
            "7790002272"
            "8121766337"
            "8130012922"
            "8310003586"
            "8381000827"
            "8520000358"
            "8520600578"
            "8722106396"
            "9231508677"))



(deftest test-check-vat-it-KO
  (are [i] (false? (a/check-ident i))
           "IT00004330545"))

 (deftest test-check-vat-it
   (are [i] (true? (a/check-vat-it i))
            "00004330544"
            "00009070285"
            "00359320264"
            "00416940476"
            "00524760246"
            "00537550261"
            "00656120367"
            "00743560153"
            "00750560211"
            "00891230153"
            "01106310939"
            "01231680156"
            "01486370404"
            "01656160692"
            "03430790240"
            "03928150402"
            "03928150402"
            "06015890012"
            "07047090159"
            "07843350153"
            "08102830018"
            "08983530158"
            "10365960151"
            "12345670785"))


(deftest test-check-vat-pt-KO
  (are [i] (false? (a/check-ident i))
           "PT503272338"))

 (deftest test-check-vat-pt
   (are [i] (true? (a/check-vat-pt i))
        "503272337"
        "501721630"
        "506510441"
        "501745815"
        "500017921"
        "500731993"
        "506000478"
        "504406400"
        "500461481"
        "504158899"
        "501393749"
        "500076090"
        "136695973"
        "500095922"
        "500163510"
        "500306486"
        "500287600"
        "501698540"
        "500002614"
        "501578102"
        "500627126"
        "500313687"
        "500287058"
        "500287058"
        "501522220"
        "502025140"
        "500020639"
        "502025140"
        "500212848"
        "500218412"
        "503425206"
        "504218581"))


(deftest test-check-vat-es-KO
  (are [i] (false? (a/check-ident i))
           "ESA04344116"))

 (deftest test-check-vat-es
   (are [i] (true? (a/check-vat-es i))
            "A04344115"
            "A08711558"
            "A20038394"
            "A20099305"
            "A28110682"
            "A30049530"
            "A30057814"
            "A30060180"
            "A31064561"
            "A31074164"
            "A31126758"
            "A33034836"
            "A46054987"
            "A47060108"
            "A58590670"
            "A78084365"
            "A79326021"
            "A83344325"
            "B02367969"
            "B11024262"
            "B11024262"
            "B17150228"
            "B17807793"
            "B18849265"
            "B20640942"
            "B20703732"
            "B21026109"
            "B21040050"
            "B21179841"
            "B21227004"
            "B30029821"
            "B30149397"
            "B30425060"
            "B30483861"
            "B30523203"
            "B30574933"
            "B41544701"
            "B43118934"
            "B50600527"
            "B55059943"
            "B61030276"
            "B62948468"
            "B65632440"
            "B72139629"
            "B73047599"
            "B73094716"
            "B73135717"
            "B73468746"
            "B73490849"
            "B73650152"
            "B73840159"
            "B78503406"
            "B81096471"
            "B91194373"
            "B91491076"
            "B92964907"
            "B98059876"
            "B98282973"
            "B98328891"
            "F18011031"
            "F30051908"
            "F30144745"))


 (deftest test-check-vat-be
   (are [i] (true? (a/check-vat-be i))
            "0136695962"
            "0308357555"
            "0401296720"
            "0404199394"
            "0405490781"
            "0407021502"
            "0407735837"
            "0414246121"
            "0419734638"
            "0425067361"
            "0432999288"
            "0437013209"
            "0441288830"
            "0441995940"
            "0446616407"
            "0448321627"
            "0449424358"
            "0449443659"
            "0459624503"
            "0462172237"
            "0465288412"
            "0474288329"
            "0475454606"
            "0478171891"
            "0478522972"
            "0479540482"
            "0818275865"
            "0822752119"
            "0887603151"
            "0896078476"))

(deftest test-check-vat-be-KO
  (are [i] (false? (a/check-ident i))
           "BE0136695963"))

(deftest test-check-ident-be
  (are [i] (true? (a/check-ident i))
           "BE0136695962"
           "BE0308357555"
           "BE0400024733"
           "BE0400135193"
           "BE0400205865"
           "BE0400661567"
           "BE0400705317"
           "BE0401264551"
           "BE0401296720"
           "BE0401299490"
           "BE0401685908"
           "BE0403476052"
           "BE0403702122"
           "BE0403764478"
           "BE0403819611"
           "BE0404199394"
           "BE0404507618"
           "BE0404535728"
           "BE0404684295"
           "BE0404782186"
           "BE0404793767"
           "BE0404811880"
           "BE0405015580"
           "BE0405161278"
           "BE0405214629"
           "BE0405270651"
           "BE0405378341"
           "BE0405456634"
           "BE0405490781"
           "BE0405525227"
           "BE0405562641"
           "BE0405581942"
           "BE0405697253"
           "BE0405716158"
           "BE0406214620"
           "BE0406315776"
           "BE0406449992"
           "BE0406456526"
           "BE0406750197"
           "BE0406811169"
           "BE0406950632"
           "BE0407021502"
           "BE0407244897"
           "BE0407688426"
           "BE0407735837"
           "BE0407801361"
           "BE0407927065"
           "BE0408053363"
           "BE0408053957"
           "BE0408080285"
           "BE0408349115"
           "BE0408391081"
           "BE0412777263"
           "BE0413152692"
           "BE0413214951"
           "BE0413619975"
           "BE0413641652"
           "BE0413756765"
           "BE0413905532"
           "BE0414246121"
           "BE0414358561"
           "BE0414620461"
           "BE0414686876"
           "BE0414966295"
           "BE0415205431"
           "BE0415561460"
           "BE0416571250"
           "BE0416790192"
           "BE0417179380"
           "BE0417938554"
           "BE0417982205"
           "BE0418624779"
           "BE0418861440"
           "BE0419661194"
           "BE0419734638"
           "BE0419812040"
           "BE0420366128"
           "BE0420383548"
           "BE0420383647"
           "BE0421712250"
           "BE0421718188"
           "BE0421871113"
           "BE0422337703"
           "BE0422588814"
           "BE0423670660"
           "BE0424099440"
           "BE0424285522"
           "BE0424342039"
           "BE0424734987"
           "BE0425067361"
           "BE0425263341"
           "BE0425916013"
           "BE0427599358"
           "BE0427693982"
           "BE0428758509"
           "BE0429476012"
           "BE0429996347"
           "BE0430677129"
           "BE0430860340"
           "BE0431114916"
           "BE0431287338"
           "BE0431847661"
           "BE0431873890"
           "BE0432820829"
           "BE0432999288"
           "BE0433005327"
           "BE0434114194"
           "BE0434332643"
           "BE0434663829"
           "BE0434905933"
           "BE0435131508"
           "BE0435533562"
           "BE0436133873"
           "BE0436273831"
           "BE0437013209"
           "BE0437711312"
           "BE0438628060"
           "BE0438924307"
           "BE0440634871"
           "BE0440950716"
           "BE0440958337"
           "BE0441288830"
           "BE0441995940"
           "BE0442928031"
           "BE0443114113"
           "BE0443598222"
           "BE0445044908"
           "BE0445560194"
           "BE0446083402"
           "BE0446251369"
           "BE0446616407"
           "BE0446854650"
           "BE0448321627"
           "BE0448324397"
           "BE0448794947"
           "BE0448840972"
           "BE0448900162"
           "BE0448967468"
           "BE0449148305"
           "BE0449424358"
           "BE0449443659"
           "BE0450118996"
           "BE0451020504"
           "BE0451594287"
           "BE0453196173"
           "BE0453926742"
           "BE0455134292"
           "BE0455238816"
           "BE0456834564"
           "BE0456963139"
           "BE0457466846"
           "BE0457847027"
           "BE0459624503"
           "BE0459793460"
           "BE0459973307"
           "BE0460069713"
           "BE0460958549"
           "BE0461116125"
           "BE0461666154"
           "BE0462015354"
           "BE0462171940"
           "BE0462172237"
           "BE0463668710"
           "BE0463936449"
           "BE0464658308"
           "BE0464857751"
           "BE0465288412"
           "BE0465897730"
           "BE0466446175"
           "BE0466569703"
           "BE0469867010"
           "BE0471882234"
           "BE0472517187"
           "BE0472680703"
           "BE0474272788"
           "BE0474288329"
           "BE0474519050"
           "BE0475222497"
           "BE0475454606"
           "BE0475900608"
           "BE0476279403"
           "BE0477341453"
           "BE0477954038"
           "BE0478107357"
           "BE0478171891"
           "BE0478223658"
           "BE0478522972"
           "BE0479540482"
           "BE0479856129"
           "BE0480434763"
           "BE0532632938"
           "BE0573735895"
           "BE0623603003"
           "BE0636605357"
           "BE0641403788"
           "BE0666232325"
           "BE0676196304"
           "BE0681111630"
           "BE0683400038"
           "BE0708690512"
           "BE0717440704"
           "BE0773046151"
           "BE0818275865"
           "BE0822752119"
           "BE0861862915"
           "BE0862209145"
           "BE0864147165"
           "BE0864618111"
           "BE0866399743"
           "BE0869530071"
           "BE0877401523"
           "BE0880498001"
           "BE0887603151"
           "BE0896078476"))


(deftest test-check-vat-hu-KO
  (are [i] (false? (a/check-ident i))
           "HU10210799"))

 (deftest test-check-vat-hu
   (are [i] (true? (a/check-vat-hu i))
            "10210798"
            "10252435"
            "10562112"
            "10813566"
            "10862508"
            "10896798"
            "11125633"
            "11168625"
            "11832700"
            "12631368"
            "12644504"
            "13323260"
            "13632142"
            "14132164"
            "17781578"))

(deftest test-check-vat-nl-KO
  (are [i] (false? (a/check-ident i))
           "NL001236363B01"))

 (deftest test-check-vat-nl
   (are [i] (true? (a/check-vat-nl i))
            "001236362B01"
            "001266706B01"
            "001266706B01"
            "001337476B01"
            "001337476B01"
            "001351746B01"
            "001524598B01"
            "001625123B01"
            "001733540B01"
            "001733540B01"
            "002345742B01"
            "002629744B01"
            "002837110B01"
            "003203098B01"
            "003360179B01"
            "003528261B01"
            "003590094B01"
            "003590094B01"
            "003813095B03"
            "004059530B01"
            "004193866B01"
            "004201991B01"
            "004297209B01"
            "004297234B01"
            "004297234B05"
            "004327822B13"
            "004407970B01"
            "004748906B01"
            "004799975B01"
            "004799975B01"
            "004799975B01"
            "004799975B01"
            "004799975B01"
            "004937338B01"
            "005286177B02"
            "005395732B01"
            "005449960B03"
            "005467755B01"
            "005652285B01"
            "005730065B01"
            "005799405B04"
            "005824813B04"
            "005928862B01"
            "006115391B01"
            "006314193B02"
            "006314193B06"
            "006575651B03"
            "006575651B03"
            "006579425B01"
            "006883011B01"
            "006918293B01"
            "006918293B01"
            "006918293B01"
            "006942027B01"
            "006953608B01"
            "006991403B01"
            "007063544B04"
            "007122998B01"
            "007156054B01"
            "007294347B03"
            "007772555B01"
            "007953525B01"
            "008165701B02"
            "008187162B05"
            "008225497B01"
            "008709282B01"
            "008740331B01"
            "008867343B01"
            "008917437B03"
            "009014251B01"
            "009143488B01"
            "009215232B01"
            "009769304B01"
            "063235833B01"
            "119965689B01"
            "235270179B01"
            "800366268B03"
            "800583668B01"
            "800669046B01"
            "800904965B05"
            "800995004B01"
            "801283929B02"
            "801283929B04"
            "801283929B04"
            "801283929B04"
            "801288794B01"
            "801427770B01"
            "801880804B01"
            "803936631B01"
            "803936631B01"
            "803936631B01"
            "805356472B01"
            "805356472B01"
            "805695187B01"
            "805695187B01"
            "805695187B01"
            "805695187B01"
            "806235093B01"
            "806639556B01"
            "806763206B06"
            "806763206B06"
            "806943476B01"
            "806943476B01"
            "807181845B01"
            "807366845B01"
            "807665538B01"
            "808248455B01"
            "808286997B01"
            "808698746B01"
            "809096444B01"
            "809203844B01"
            "809203844B01"
            "810411970B01"
            "810617511B01"
            "810617511B01"
            "810717360B01"
            "811514274B01"
            "811514274B01"
            "811514274B01"
            "811514274B01"
            "817911510B01"))


(deftest test-check-vat-se-KO
  (are [i] (false? (a/check-ident i))
           "SE000000003501"))


 (deftest test-check-vat-se
   (are [i] (true? (a/check-ident i))
        "SE000000003401"
        "SE000000006701"
        "SE000000008301"
        "SE000000011701"
        "SE000000014101"
        "SE000000016601"
        "SE000000019001"
        "SE136695975523"
        "SE202100287401"
        "SE202100297301"
        "SE202100306201"
        "SE202100321101"
        "SE262000119401"
        "SE502052817901"
        "SE502069927701"
        "SE516403812601"
        "SE556035133901"
        "SE556126249301"
        "SE556263276901"
        "SE556399449901"
        "SE556500060001"
        "SE556576895801"
        "SE556785615701"))


 (deftest test-check-vat-el
   (are [i] (true? (a/check-vat-el i))
        "123456783"
        "095754514"
        "999010939"
        "094450902"))

(deftest test-check-ident-el-ko
  (are [i] (false? (a/check-ident i))
           "EL094012835"))


(deftest test-check-ident-el
  (are [i] (true? (a/check-ident i))
           "EL000000036"
           "EL000000073"
           "EL000000104"
           "EL000000128"
           "EL000000153"
           "EL000000189"
           "EL000000208"
           "EL000000208"
           "EL055679750"
           "EL073313955"
           "EL090077522"
           "EL094012834"
           "EL094112730"
           "EL094237076"
           "EL094253457"
           "EL094322222"
           "EL094403384"
           "EL098002010"
           "EL099370743"
           "EL800420948"
           "EL997786906"
           "EL998180212"
           "EL998920231"))

 (deftest test-check-vat-ie
   (are [i] (true? (a/check-vat-ie i))
        "6599525K"
        "4870800A"
        "6322073C"
        "8F47910T"
        "4574973H"
        "8F85560M"
        "9F69313D"
        "8F61758E"
        "4571931W"
        "4809539S"
        "6589960W"
        "6569740T"
        "4809539S"
        "9568085J"
        "6586063A"
        "6553007D"
        "4620030C"
        "8236506T"
        "4799334B"
        "0526750E"
        "4536860D"))


(deftest test-check-ident-ie-ko
  (are [i] (false? (a/check-ident i))
           "IE1409095O"
           "IE4749148S"))

(deftest test-check-ident-ie
  (are [i] (true? (a/check-ident i))
           "IE0000003F"
           "IE0000007N"
           "IE0000010C"
           "IE0000012G"
           "IE0000015M"
           "IE0000018S"
           "IE0000020F"
           "IE0000020F"
           "IE1409095C"
           "IE4749148U"
           "IE4873338U"
           "IE6334989A"
           "IE6336982T"
           "IE6344439R"
           "IE6387098K"
           "IE6409194V"
           "IE6426706T"
           "IE6517957E"
           "IE6556973V"
           "IE6570116F"
           "IE8213349C"
           "IE8223184C"
           "IE8232698L"
           "IE8E86432H"
           "IE9578054E"
           "IE9694881P"
           "IE9700053D"
           "IE9800871V"
           "IE9E61585W"
           "IE9F70164P"
           "IE1113202EH"
           "IE1113571MH"
           "IE1113778LH"
           "IE2974611LH"
           "IE3200115LH"
           "IE3208913KH"))

 (deftest test-check-vat-at
   (are [i] (true? (a/check-vat-at i))
        "U13585627"
        "U30164005"
        "U34260000"
        "U34699207"
        "U51025000"
        "U35613608"
        "U35613608"
        "U35613608"
        "U15393205"
        "U29074701"
        "U46153102"
        "U15301206"
        "U46234105"
        "U44463503"
        "U19208701"
        "U19208701"
        "U19208701"
        "U19208701"
        "U20785309"
        "U35613608"
        "U46234105"
        "U15383403"))


 (deftest test-check-vat-is
   (are [i] (true? (a/check-vat-is i))
        "23311347"
        "23191503"
        "64665020"
        "56405006"
        "18805477"
        "81959745"
        "77779517"
        "72161248"
        "16519949"
        "52690644"
        "46695966"))

 (deftest test-check-vat-fi
   (are [i] (true? (a/check-ident i))
        "FI13669598"
        "FI00000027"
        "FI00000043"
        "FI00000078"
        "FI00000094"
        "FI00000123"
        "FI00000166"
        "FI00000182"
        "FI00000211"
        "FI01244162"
        "FI06312080"
        "FI09441865"
        "FI10154054"
        "FI15380325"
        "FI15482348"
        "FI16802358"
        "FI17405469"
        "FI18261444"
        "FI20303674"
        "FI22283574"
        "FI22780669"
        "FI22975669"
        "FI24710461"))

 (deftest test-check-vat-lu
   (are [i] (true? (a/check-vat-lu i))
        "17458960"
        "14492937"
        "11742635"
        "12192281"
        "13083505"
        "14492937"
        "16965218"
        "17458960"
        "18147908"
        "15959114"
        "15068003"
        "18329342"
        "15349155"
        "10522325"
        "18028257"
        "19803611"
        "19088973"
        "18668754"
        "17458960"
        "17458960"
        "14922662"
        "17044510"
        "18970647"
        "21623857"
        "16468636"))


(deftest test-check-vat-bg
  (are [i] (true? (a/check-ident i))
           "BG030215845"
           "BG102146713"
           "BG121446525"
           "BG130465173"
           "BG175200037"
           "BG201271946"
           "BG201820473"
           "BG202188510"
           "BG202272022"))

(deftest test-check-vat-lt
  (are [i] (true? (a/check-ident i))
           "LT100001123912"
           "LT100002058914"
           "LT100005407518"
           "LT401688219"
           "LT501643716"
           "LT633770418"))

(deftest test-check-vat-si
  (are [i] (true? (a/check-ident i))
           "SI16519949"
           "SI18805477"
           "SI23191503"
           "SI23311347"
           "SI38211670"
           "SI39373789"
           "SI40370879"
           "SI41200900"
           "SI46695966"
           "SI47716878"
           "SI51033224"
           "SI52690644"
           "SI54221749"
           "SI56405006"
           "SI60031751"
           "SI64665020"
           "SI72161248"
           "SI77048008"
           "SI77779517"
           "SI80901042"
           "SI81959745"))


(deftest test-check-vat-sk
  (are [i] (true? (a/check-ident i))
           "SK2020180580"
           "SK2020295156"
           "SK2020298610"
           "SK2020321655"
           "SK2020326759"
           "SK2020393749"
           "SK2020410282"
           "SK2020491374"
           "SK2021788879"
           "SK2021869267"
           "SK2022087540"
           "SK2022096901"
           "SK2022704805"
           "SK2022787569"
           "SK2022916819"))


(deftest test-check-vat-lv
  (are [i] (true? (a/check-ident i))
           "LV40003649404"
           "LV40003009497"
           "LV50003993021"
           "LV40003009497"
           "LV40003048583"
           "LV40003130421"
           "LV40003224680"
           "LV40003275598"
           "LV40003282138"
           "LV40003348054"
           "LV40003439368"
           "LV40003511655"
           "LV40003568404"
           "LV40003585673"
           "LV40003651875"
           "LV40003732964"
           "LV40003857687"
           "LV40008000225"
           "LV40103058465"
           "LV40103247567"
           "LV40103446084"
           "LV40103619251"
           "LV41031037436"
           "LV50003017621"
           "LV50008111541"
           "LV90000136794"))


(deftest test-check-vat-mt
  (are [i] (true? (a/check-ident i))
           "MT15121333"
           "MT10988628"
           "MT10271622"
           "MT10414318"
           "MT10830531"
           "MT11012007"
           "MT11407334"
           "MT11612810"
           "MT12041610"
           "MT12667313"
           "MT12894031"
           "MT14024410"
           "MT14632420"
           "MT15750503"
           "MT15903903"
           "MT16509511"
           "MT16657432"
           "MT16910734"
           "MT17727224"
           "MT18177531"
           "MT19420526"
           "MT19738201"
           "MT20250021"
           "MT20973507"))


#_(deftest test-check-ident-no
  (are [i] (true? (a/check-vat-no i))
           "NO15121333"))



(deftest test-check-ident-che
  (are [i] (true? (a/check-ident i))
           "CHE100738566"
           "CHE105778633"
           "CHE105880361"
           "CHE105930993"
           "CHE105950470"
           "CHE107698407"
           "CHE116287842"
           "CHE280763979"))


(deftest test-check-ident-cy
  (are [i] (true? (a/check-ident i))
           "CY00532445O"
           "CY00001067Y"
           "CY00376309R"
           "CY00506026O"
           "CY00709533C"
           "CY00714754A"
           "CY10000314J"
           "CY10000463Y"
           "CY10008146K"
           "CY10008489A"
           "CY10018402C"
           "CY10030661B"
           "CY10030954F"
           "CY10110278D"
           "CY10111176Z"
           "CY10111474A"
           "CY10156988E"
           "CY10157423I"
           "CY10165829P"
           "CY10166866Y"
           "CY10173610U"
           "CY10188550T"
           "CY10221051V"
           "CY10227520I"
           "CY10231803U"
           "CY10244276R"
           "CY10247148S"
           "CY10259033P"
           "CY10259584H"
           "CY10265331J"
           "CY10269393H"
           "CY10272781S"
           "CY10272781S"
           "CY10274481T"
           "CY10283929R"
           "CY30009560X"
           "CY90000265T"
           "CY90000448S"
           "CY90002066W"
           "CY99000027S"
           "CY99200002N"))


(deftest test-check-ident-hr
  (are [i] (true? (a/check-ident i))
           "HR30648279396"
           "HR33392005961"
           "HR29571564403"
           "HR41259773329"
           "HR75735401567"
           "HR52394886229"
           "HR06282943396"
           "HR08308894711"
           "HR12385860076"
           "HR16364086764"
           "HR17099025134"
           "HR20963249418"
           "HR22910368449"
           "HR24595836665"
           "HR25107893471"
           "HR28922587775"
           "HR39672837472"
           "HR46144176176"
           "HR61867710134"
           "HR69715301002"
           "HR81592331325"
           "HR82067332481"
           "HR85760419184"
           "HR89018712265"
           "HR91025164621"
           "HR93634429487"
           "HR96151551854"))

(deftest test-check-ro
  (are [i] (true? (a/check-ident i))
           "RO9270326"
           "RO18111664"
           "RO9449810"
           "RO24218836"
           "RO30628784"
           "RO6614115"
           "RO11168477"
           "RO14478022"
           "RO22014732"
           "RO17802998"
           "RO27673947"
           "RO1590678"
           "RO2139251"))



(time
  (run-tests))



