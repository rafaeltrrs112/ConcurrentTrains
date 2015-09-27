/**
 * This example actually uses the FXML file and binds
 * it's logic to the FXML file's generated GUI
 */
import javafx.beans.binding.StringBinding
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ComboBox, TextField}
import scalafx.scene.shape.Circle
import scalafxml.core.macros.sfxml
import scalafxml.core.{DependenciesByType, FXMLView}

//@sfxml
class ConcurrentTrainsPresenterX(
  private val from: TextField,
  private val to: TextField,
  private val types: ComboBox[UnitConverter],
  private val converters : UnitConverters){

  //Filling the combo box
  for (converter <- converters.available){
    types += converter
  }
  types.getSelectionModel.selectFirst()

  //Data binding
  to.text <== new StringBinding{
    bind(from.text.delegate, types.getSelectionModel.selectedItemProperty)
    def computeValue() = types.getSelectionModel.getSelectedItem.run(from.text.value)
  }

  def onClose(event : ActionEvent): Unit ={
    Platform.exit()
  }
}
//object ScalaFXML extends JFXApp {
//  val root = FXMLView(getClass.getResource("src/main/unitconverter.fxml"),
//    new DependenciesByType(Map(
//      typeOf[UnitConverters] -> new UnitConverters(InchesToMM, MMtoInches))))
//
//  stage = new PrimaryStage(){
//    title = "Unit conversion"
//    scene = new Scene(root)
//
//  }
//}
//
//@sfxml
//class ConcurrentTrainsPresenterX(
//                               private val northDispatch: Button,
//                               private val northPassenger : Label,
//                               private val northStatus : Circle,
//                               private val northCount : Label,
//                               private val southDispatch: Button,
//                               private val southPassenger : Label,
//                               private val southStatus : Circle,
//                               private val southCount : Label
//                               ){
//
//}
//object ConcurrentTrainsFXML extends JFXApp{
//  val root = FXMLView(getClass.getResource("concurrentrains.fxml"),
//    new DependenciesByType(Map(
//      typeOf[UnitConverters] -> new UnitConverters(InchesToMM, MMtoInches))))
//
//  stage = new PrimaryStage(){
//    title = "Unit conversion"
//    scene = new Scene(root)
//  }
//}
