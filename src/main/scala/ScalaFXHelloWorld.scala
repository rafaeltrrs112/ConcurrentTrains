import javafx.beans.value.ObservableValue
import javafx.beans.value.{ObservableValue, ObservableStringValue}

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.Platform.runLater
import scalafx.beans.value.ObservableValue
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.effect.DropShadow
import scalafx.beans.Observable
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.text.Text
import scalafx.Includes._
import scalafx.scene.layout.BorderPane
import scalafx.scene.control.Button
import scalafx.application.Platform
import scalafx.beans.property.StringProperty

object ScalaFXHelloWorld extends JFXApp {
  class Model(var text : StringProperty)
  val stringProperty = new Model(new StringProperty(this, "stringProperty", "initial"))
  stage = new PrimaryStage {
    title = "ScalaFX Hello World"
    stage = new PrimaryStage{
      scene = new Scene {
        root = new BorderPane {
          padding = Insets(20)
          center = new Label{
             text <== stringProperty.text
          }
          center.onChange()
          left = new Button("Click Me!"){
            onMouseClicked = (me: MouseEvent) => {
              //Platform.runLater takes in a function by name and adds it to the
              //Event Dispatch Thread asynchronously.

              //To use this with actors have an actor in charge of some text field on the screen.
              //As the actors do their jobs they will update their
              //Documentation : https://docs.oracle.com/javase/8/javafx/api/javafx/application/Platform.html
              //Warning use this for simple GUI updates only
              //Do not initiate any logic on here
              runLater{
                stringProperty.text.set("After")
                println("This button was clicked")
              }
            }
          }
        }
      }
    }
  }
}