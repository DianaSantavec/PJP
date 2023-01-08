object AppMain{
    def main(args : Array[String]) : Unit ={

        var listaSkola : List[Skola] = List(
            new OsnovnaSkola("OS1", 10),
            new SrednjaSkola("SR1", 31),
            new OsnovnaSkola("OS2", 58),
            new SrednjaSkola("SR2", 999),
            new SrednjaSkola("SR3", 645),
        )

        //odrediTipSkole(listaSkola)
        //odrediTipSkole(izbaciManjeSkole(listaSkola, 58))

        var listaNebeskihTela : List[NebeskoTelo] = List(
            new Planeta("Planeta1", 3.3, false),
            new Planeta("Planeta2", 5555, true),
            new Satelit("Satelit1", 1, "Planeta1")
        )

        //println(satelitiIzOrbitePlanete(listaNebeskihTela, "Planeta1"))
        println(filtrirajPlaneteISatelite(listaNebeskihTela))
    }

//1)
    abstract class Skola(
        val ime : String,
        val brojUcenika : Int
    )

    case class OsnovnaSkola (
        x : String,
        y : Int
    ) extends Skola (x, y)

    case class SrednjaSkola (
        x : String,
        y : Int
        ) extends Skola (x,y)

// 2)
    def odrediTipSkole(skole : List[Skola]) : Unit ={
        skole match{
            case Nil => return
            case head::tail => 
                head match {
                    case OsnovnaSkola(ime, brojUcenika) =>{
                        println(s"Osnovna skola $ime ima $brojUcenika broj ucenika")
                        odrediTipSkole(tail)
                    }
                    case SrednjaSkola(ime, brojUcenika) => {
                        println(s"Srednja skola $ime ima $brojUcenika broj ucenika")
                        odrediTipSkole(tail)
                    }
                }
        }
    }

// 3)
    def izbaciManjeSkole(skole : List[Skola], minimalanBrojUcenika : Int) : List[Skola] ={
        skole.filter((skola : Skola) => skola.brojUcenika >= minimalanBrojUcenika)
    }

// 4)
    abstract class NebeskoTelo(
        val ime : String,
        val precnik : Double
    )

    case class Planeta (
        val i : String,
        val p : Double,
        val imaVodu : Boolean
    ) extends NebeskoTelo (i, p)

    case class Satelit(
        val i : String,
        val p : Double,
        val kruziOko : String
    ) extends NebeskoTelo (i, p)

// 5)
    def satelitiIzOrbitePlanete(nebeskaTela : List[NebeskoTelo], imePlanete : String) : List[NebeskoTelo] =
        nebeskaTela.filter(satelitiOko(imePlanete))

    def satelitiOko (imePlanete: String)(nebeskoTelo : NebeskoTelo) =
        nebeskoTelo match {
            case Satelit (i, p, kruziOko) =>
                kruziOko.equals(imePlanete)
            case _ =>
                false
        }

// 6)
    def filtrirajPlaneteISatelite(nebeskaTela : List[NebeskoTelo]) : List[NebeskoTelo] ={
        nebeskaTela match{
            case Nil => Nil
            case head::tail =>
                head match{
                    case Planeta(i, p, imaVodu) =>
                        if(imaVodu && p > 5500)
                            head :: filtrirajPlaneteISatelite(tail)
                        else
                            filtrirajPlaneteISatelite(tail)
                    
                    case Satelit(i, p, kruziOko) =>
                        if (p < 1000)
                            head :: filtrirajPlaneteISatelite(tail)
                        else
                            filtrirajPlaneteISatelite(tail)
                }
        }
    }
    

}