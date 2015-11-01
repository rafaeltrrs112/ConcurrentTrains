import assignmenttwo.pokemon.{Flareon, Garydos, Leafeon, Zapdos}
import assignmenttwo.server.Arena
import scalafx.scene.control.{Label, ProgressBar}
import scala.reflect.runtime.universe.typeOf
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.Scene
import scalafx.scene.shape.Circle
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
//  for(i <- 1 to 50){
//    leafeonBar.progress.set(i / 50)
//    Thread.sleep(500)
//  }
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

