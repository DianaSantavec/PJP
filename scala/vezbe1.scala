import scala.io.StdIn._

object AppMain{
    def main(args: Array[String]) : Unit = {
        //var slovo : Char = 'a'
        //println(tipKaraktera(slovo))

        //println(pomnoziSa(5, Array(2,3)))

        //val kvadrirani = kvadriraj(Array(2,3,5))
        //for (broj <- kvadrirani)
        //    println(broj.toString)

        //ispisiVisePuta ('f', 4)

        /*var prviBroj = readInt()
        var drugiBroj = readInt()
        var znak = readChar()

        var rezultat = kalkulator(prviBroj,drugiBroj,znak)
        println(rezultat)*/

        /*println(proveraBrojevnogTipa(5))
        println(proveraBrojevnogTipa(5.5))
        println(proveraBrojevnogTipa(5.5f))
        println(proveraBrojevnogTipa('c'))*/

        println(transformisiPar((2,4),5))        

    }

// 1)
    def tipKaraktera (karakter: Char) : String ={
        if (karakter >= 'a' && karakter <= 'z')
            "Malo slovo"
        else if (karakter >= 'A' && karakter <= 'Z')
            "Veliko slovo"
        else
            "Nije slovo"
    }
// 2)
    def pomnoziSa (prvi : Int, niz : Array[Int]) : Int ={
        var rezultat = prvi
        for ( element <- niz){
            rezultat *= element
        }
        rezultat
    }
// 3)
    def kvadriraj (niz : Array[Int]) : Array[Int] ={
        for (broj <- niz)
            yield broj*broj
    }
// 4)
    def ispisiVisePuta (karakter : Char, brojPuta : Int)  ={
        var i : Int = 0
        while (i < brojPuta){
            print(s"$karakter ");
            i += 1
        }
    }

// 5)
    def kalkulator (prviBroj : Int, drugiBroj : Int, znak : Char) : Int ={
        znak match{
            case '+' => prviBroj + drugiBroj;
            case '-' => prviBroj - drugiBroj;
            case '*' => prviBroj * drugiBroj;
            case '/' => prviBroj / drugiBroj;
            case _ => 0;
        }
    }

// 6)
    def proveraBrojevnogTipa(a :Any): String ={
        a match{
            case n : Int => "Int"
            case n : Float => "Float"
            case n : Double => "Double"
            case _ => "Nije broj"
        }
    }

// 7)
    def transformisiPar(par : Tuple2[Int, Int], n : Int) : Tuple2[Int, Int] ={
        (par._1 * n, par._2 + n)
    }

// 8)
    def spoljosti(lista : List[Int]) : List[Int] ={
        l match{
            case Nil => Nil
            case (x: Int) :: Nil => x :: Nil
            case (x: Int) :: (y:Int) :: tail =>
                if (x == y)
                    spoljosti(y :: tail)
                else
                    x :: spoljosti(y::tail)
        }
    }
}

