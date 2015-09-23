/**
 * This is an example application where I use just
 * scalaFX to make a gui.
 */
import javafx.beans.binding.StringBinding
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.event.ActionEvent
import scalafx.geometry.{HPos, Insets}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ComboBox, Label, TextField}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{ColumnConstraints, GridPane, Priority}
import scalafxml.core.macros.sfxml


trait UnitConverter{
  val description : String

  /**
   *
   * @param input
   *              A string representation of the number due for conversion
   * @return
   *         The result of the conversion represented as a string
   */
  def run(input: String) : String

  override def toString = description
}

object MMtoInches extends UnitConverter {
  val description : String = "Millimeters to inches"

  override def run(input: String): String = {
    try {
      (input.toDouble / 25.4).toString
    } catch {
      case ex: Throwable => ex.toString;
    }
  }
}

object InchesToMM extends UnitConverter {
  val description : String = "Inches to millimeters"
  def run(input : String) : String = {
    try {
      (input.toDouble * 25.4).toString

    } catch {
      case ex : Throwable => "Please Enter a Valid Number"
    }
  }
}
//Unit converters helper class that contains all the
//unit converters within it's available converters property
class UnitConverters(converters : UnitConverter*){
  val available = List(converters : _*)
}

//Now let's implement our view using pure scalafx with no GUI helper
/**
 *
 * @param converters
 *                   Pass the wrapper class for a collection of unit conversions
 */
class PureSCalaFXView(converters : UnitConverters) extends JFXApp.PrimaryStage{

  //UI Definition
  title = "Unit Conversion"

  /**
   * combo box containing UnitConverter objects
   */
  private val types  = new ComboBox[UnitConverter](){
    maxWidth = Double.MaxValue
    margin = Insets(3)
  }

  /**
   * Text field where user enters the value due for conversion
   */
  private val from = new TextField {
    margin = Insets(3)
    prefWidth = 200.0
  }

  /**
   * Un-editable text box that holds the result of the conversion
   */
  private val to = new TextField {
    margin = Insets(3)
    prefWidth = 200.0
    editable = false
  }

  private val presenter = new RawUnitConverterPresenter(from, to, types, converters)

  scene = new Scene {
    content = new GridPane {
      padding = Insets(5)

      add(new Label("Conversion type"), 0, 0)
      add(new Label("From:"), 0, 1)
      add(new Label("To:"), 0, 2)

      add(types, 1, 0)
      add(from, 1, 1)
      add(to, 1, 2)

      add(new Button{
        text = "Close"
        onMouseClicked = (_ : MouseEvent) => {Platform.exit()
        }
      }, 1, 3)

      //Column constraints are stored in a list. On constraint for each column
      columnConstraints = List(
      new ColumnConstraints {
        halignment = HPos.LEFT
        hgrow = Priority.SOMETIMES
        margin = Insets(5)
      },
      new ColumnConstraints{
        halignment = HPos.RIGHT
        hgrow = Priority.ALWAYS
        margin = Insets(5)
      }
      )
    }
  }
}
class RawUnitConverterPresenter(
                               private val from : TextField,
                               private val to: TextField,
                               private val types: ComboBox[UnitConverter],
                               private val converters : UnitConverters){

  //Filling the combo box
  for(converter <- converters.available){
    types += converter
  }
  types.getSelectionModel.selectFirst()
  //Low level data binding
  //bind takes a set of dependencies that are needed for computeValue to execute
  //The dependencies are observed when computeValue is called which is all the time.
  to.text <== new StringBinding {
    bind(from.text.delegate, types.getSelectionModel.selectedItemProperty)
    def computeValue() = types.getSelectionModel.getSelectedItem.run(from.text.value)
  }


  def onClose(event : ActionEvent): Unit ={
    Platform.exit()
  }
}

object PureScalaFX extends JFXApp {
  stage = new PureSCalaFXView(new UnitConverters(InchesToMM, MMtoInches))
}
