import assignmenttwo.pokemon.{Flareon, Garydos, Leafeon, Zapdos}
import assignmenttwo.server.Arena
import org.openjdk.jmh.annotations.{OutputTimeUnit, Mode, BenchmarkMode, Benchmark}
import scalafx.scene.control.{ ProgressBar}
import scala.reflect.runtime.universe.typeOf
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.Scene
import scalafxml.core.macros.sfxml
import scalafxml.core.{DependenciesByType, FXMLView}

object PokeView {
  var barMap = scala.collection.immutable.Map[String, ProgressBar]()
  var arenaBuffer = scala.collection.mutable.ArrayBuffer[Arena]()
}

@sfxml
class PokemonPresenter(private val leafeonBar : ProgressBar,
                                private val garydosBar: ProgressBar,
                                private val flareonBar: ProgressBar,
                                private val zapdosBar: ProgressBar
                                 ) {
  val battleOne = new Arena("ArenaOne", Flareon("Flareon", 10000, 500, flareonBar), Leafeon("Leafeon", 10000, 500, leafeonBar), (leafeonBar, flareonBar))
  val battleTwo = new Arena("ArenaTwo", Zapdos("Zapdos", 10000, 500, zapdosBar), Garydos("Garydos", 10000, 500, garydosBar), (zapdosBar, garydosBar))
  PokeView.barMap = collection.immutable.Map[String, ProgressBar]("Garydos" -> garydosBar, "Flareon" -> flareonBar, "Zapdos" -> zapdosBar, "Leafeon" -> leafeonBar)
  //Sleep the thread while the UI builds and binds

  Platform.runLater{battleOne.start()}
  Platform.runLater{battleTwo.start()}
}

object PokemonFXML extends JFXApp {
  val root = FXMLView(getClass.getResource("pokemon.fxml"),
    new DependenciesByType(Map(
      typeOf[UnitConverters] -> new UnitConverters(InchesToMM, MMtoInches))))

  stage = new PrimaryStage() {
    title = "Pokemon Arena"
    scene = new Scene(root)
  }
}


class test {

  def testActual() : Unit = {
    val battleOne = new Arena("ArenaOne", Flareon("Flareon", 10000, 500, null), Leafeon("Leafeon", 10000, 500, null), (null, null))
    val battleTwo = new Arena("ArenaTwo", Zapdos("Zapdos", 10000, 500, null), Garydos("Garydos", 10000, 500, null), (null, null))
    PokeView.barMap = collection.immutable.Map[String, ProgressBar]("Garydos" -> null, "Flareon" -> null, "Zapdos" -> null, "Leafeon" -> null)

    Platform.runLater{battleOne.start()}
    Platform.runLater{battleTwo.start()}
  }
}

