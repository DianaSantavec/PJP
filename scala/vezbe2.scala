import scala.io.StdIn._

object AppMain{
    def main(args : Array[String]) : Unit ={
        //for (i <- 1 to 10)
        //    println(i)

        //for (i <- 1 until 10)
        //    print(s"$i ")

        //for (i <- 3 to 10 by 3)
        //    print(i)

        var lista1 = List(1,2,3)
        var lista2 = List(4,5,6,7)
        var lista3 = List(1,2,4,5,7,8,10, 12)
        var listaTest = List(lista1, lista2, lista3)
        
        var listaTestString = List("Hello", "world")

        //println(ispeglaj(listaTest))
        //println(sumirajPodliste(listaTest))
        //println(izbaciParne(listaTest))
        //println(okreni(listaTestString))
        //println(izbaciDeljiveSa3(listaTest))
        println(izbaciListeSaManjeOd5BrojevaKojiNisuDeljiviSa3(listaTest))


    }

// 1)
    def ispeglaj(lista : List[List[Int]]) : List[Int] ={
        lista.reduce((x,y) => x ++ y)
    }

// 2)
    def sumirajPodliste(lista : List[List[Int]]) : List[Int] ={
        lista.filter(x => x != Nil)
            .map(podlista => podlista.reduce(_+_))
    }

// 3)
    def izbaciParne(lista : List[List[Int]]) : List[List[Int]] ={
        lista.map(podlista => podlista.filter(x => x % 2 != 0))
            .filter(novaLista => novaLista != Nil)
    }

// 4)
    def okreni(lista : List[String]) : List[String] ={
        lista.map(podlista => podlista.reverse)
    }

// 5)
    def izbaciDeljiveSa3(lista : List[List[Int]]) : List[List[Int]] ={
        lista.map(podlista => podlista.filter(x => x % 3 != 0))
    }

// 6)
    def izbaciListeSaManjeOd5BrojevaKojiNisuDeljiviSa3(lista : List[List[Int]]) : List[List[Int]] ={
        izbaciDeljiveSa3(lista).filter(x => x.length >=5)
    }

}