package quanto.gui

import java.awt.{Color, Shape}

import org.lindenb.svg.SVGUtils
import quanto.data.Theory._
import quanto.data.{CompositeExpression, GenericParseException, Theory, TheoryLoadException}
import quanto.util.UserAlerts.{Elevation, alert}
import quanto.util.UserOptions.scaleInt
import quanto.util._
import quanto.util.json.{Json, JsonObject}
import quanto.util.swing.ToolBar

import scala.language.postfixOps

import scala.swing._
import scala.swing.event.{ButtonClicked, SelectionChanged}

class TheoryEditPanel() extends BorderPanel with HasDocument {
  val document = new TheoryDocument(this)
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  val Toolbar = new ToolBar {
    //contents
  }
  val EditorsCombined = new BoxPanel(Orientation.Vertical)
  val TopScrollablePane = new ScrollPane(EditorsCombined)

  implicit private def valueTypeVectorToString(vs: Vector[ValueType]) : String =
    vs.map(approvedDataTypes).mkString("",", ","")

  val approvedDataTypes: Map[ValueType, String] = ValueType.values.toList map (v =>
    v -> (v match {
    case ValueType.AngleExpr => "angle"
    case ValueType.Boolean => "boolean"
    case ValueType.Integer => "integer"
    case ValueType.Rational => "rational"
    case ValueType.String => "string"
    case ValueType.Long => "long"
    case ValueType.Enum => "string"
    case ValueType.Empty => "empty"
  })) toMap

  val approvedColours: Map[String, Color] = Map(
    "red" -> colourMute(new Color(255, 0, 0)),
    "green" -> colourMute(new Color(0, 255, 0)),
    "blue" -> colourMute(new Color(0, 0, 255)),
    "white" -> colourMute(new Color(255, 255, 255)),
    "black" -> colourMute(new Color(0, 0, 0)),
    "yellow" -> colourMute(new Color(255, 255, 0)),
    "magenta" -> colourMute(new Color(255, 0, 255)),
    "cyan" -> colourMute(new Color(0, 255, 255))
  )

  def colourMute(c: Color): Color = {
    def m(i: Int): Int = math.floor(i * 1).toInt

    new Color(m(c.getRed), m(c.getGreen), m(c.getBlue))
  }


  def chooseNodeDataType(node: String, current: String): Unit = {
    val dialog = new DataTypePickingDialog(node, current)
    dialog.centerOnScreen()
    dialog.open()
    if(dialog.wasAccepted){
      val typeSelected = dialog.DataComboBox.selection.item
      val newTypeVector: Vector[ValueType] = if(typeSelected == "composite"){
        try{
          CompositeExpression.parseTypes(dialog.CustomText.text)
        } catch {
          case _ : GenericParseException => Vector()
        }
      } else {
        CompositeExpression.parseTypes(typeSelected)
      }


      val oldVertexDesc = theory.vertexTypes(node)
      val newTypeValue : ValueDesc = new ValueDesc(typ = newTypeVector,
        oldVertexDesc.value.enumOptions,
        oldVertexDesc.value.latexConstants,
        oldVertexDesc.value.validateWithCore)

      val newVertexDesc : VertexDesc = new VertexDesc(newTypeValue,
        oldVertexDesc.style,
        oldVertexDesc.defaultData)

      val newVertexTypes: Map[String, VertexDesc] = theory.vertexTypes + (node -> newVertexDesc)
      // Update the base document's theory
      theory = new Theory(name = theory.name,
        coreName = theory.coreName,
        vertexTypes = newVertexTypes,
        edgeTypes = theory.edgeTypes,
        defaultVertexType = theory.defaultVertexType,
        defaultEdgeType = theory.defaultEdgeType
      )
    }
  }

  def chooseNodeColour(node: String, current: Color): Unit = {
    val dialog = new ColourPickingDialog(node, current)
    dialog.centerOnScreen()
    dialog.open()
    val newColourHex: String = dialog.CustomText.text
    val newColour = Color.decode(newColourHex)
    val oldStyle: VertexStyleDesc = theory.vertexTypes(node).style
    val newStyle: VertexStyleDesc = new VertexStyleDesc(
      shape = oldStyle.shape,
      customShape = oldStyle.customShape,
      strokeColor = oldStyle.strokeColor,
      fillColor = newColour,
      labelPosition = oldStyle.labelPosition,
      labelForegroundColor = oldStyle.labelForegroundColor,
      labelBackgroundColor = oldStyle.labelBackgroundColor
    )

    val oldVertexDesc = theory.vertexTypes(node)
    val newVertexDesc = new VertexDesc(oldVertexDesc.value,
      newStyle,
      oldVertexDesc.defaultData)

    val newVertexTypes: Map[String, VertexDesc] = theory.vertexTypes + (node -> newVertexDesc)
    // Update the base document's theory
    theory = new Theory(name = theory.name,
      coreName = theory.coreName,
      vertexTypes = newVertexTypes,
      edgeTypes = theory.edgeTypes,
      defaultVertexType = theory.defaultVertexType,
      defaultEdgeType = theory.defaultEdgeType
    )
  }

  // Method for choosing the strokecolour of an edge
  def chooseEdgeColour(edge: String, current: Color): Unit = {
    val dialog = new ColourPickingDialog(s"Choose colour for edge $edge", current)
    dialog.centerOnScreen()
    dialog.open()
    val newColourHex: String = dialog.CustomText.text
    val newColour = Color.decode(newColourHex)
    val oldStyle: EdgeStyleDesc = theory.edgeTypes(edge).style
    val newStyle: EdgeStyleDesc = new EdgeStyleDesc(
      strokeColor = newColour,
      strokeWidth = oldStyle.strokeWidth,
      labelPosition = oldStyle.labelPosition,
      labelForegroundColor = oldStyle.labelForegroundColor,
      labelBackgroundColor = oldStyle.labelBackgroundColor
    )

    val oldEdgeDesc = theory.edgeTypes(edge)
    val newEdgeDesc = new EdgeDesc(oldEdgeDesc.value,
      newStyle,
      oldEdgeDesc.defaultData)

    val newEdgeTypes: Map[String, EdgeDesc] = theory.edgeTypes + (edge -> newEdgeDesc)
    // Update the base document's theory
    theory = new Theory(name = theory.name,
      coreName = theory.coreName,
      vertexTypes = theory.vertexTypes,
      edgeTypes = newEdgeTypes,
      defaultVertexType = theory.defaultVertexType,
      defaultEdgeType = theory.defaultEdgeType
    )
  }

  // Method for choosing the strokewidth of an edge
  def chooseEdgeWidth(edge: String, current: Int): Unit = {
    val dialog = new EdgeWidthDialog(edge, current)
    dialog.centerOnScreen()
    dialog.open()
    val newSize = dialog.PlacementComboBox.selection.item.toInt
    val oldStyle: EdgeStyleDesc = theory.edgeTypes(edge).style
    val newStyle: EdgeStyleDesc = new EdgeStyleDesc(
      strokeColor = oldStyle.strokeColor,
      strokeWidth = newSize,
      labelPosition = oldStyle.labelPosition,
      labelForegroundColor = oldStyle.labelForegroundColor,
      labelBackgroundColor = oldStyle.labelBackgroundColor
    )

    val oldEdgeDesc = theory.edgeTypes(edge)
    val newEdgeDesc = new EdgeDesc(oldEdgeDesc.value,
      newStyle,
      oldEdgeDesc.defaultData)

    val newEdgeTypes: Map[String, EdgeDesc] = theory.edgeTypes + (edge -> newEdgeDesc)
    // Update the base document's theory
    theory = new Theory(name = theory.name,
      coreName = theory.coreName,
      vertexTypes = theory.vertexTypes,
      edgeTypes = newEdgeTypes,
      defaultVertexType = theory.defaultVertexType,
      defaultEdgeType = theory.defaultEdgeType
    )
  }

  // Choose label position for current node
  def chooseLabelPlacement(node: String, current: VertexLabelPosition): Unit = {
    val dialog = new NodeLabelPlacementDialog(node, current)
    dialog.centerOnScreen()
    dialog.open()
    val newPlacementName: String = dialog.PlacementComboBox.item
    val newPlacement: VertexLabelPosition = VertexLabelPosition.fromName(newPlacementName).
      getOrElse(VertexLabelPosition.values.head)
    val oldStyle: VertexStyleDesc = theory.vertexTypes(node).style
    val newStyle: VertexStyleDesc = new VertexStyleDesc(
      oldStyle.shape,
      oldStyle.customShape,
      strokeColor = oldStyle.strokeColor,
      fillColor = oldStyle.fillColor,
      labelPosition = newPlacement,
      labelForegroundColor = oldStyle.labelForegroundColor,
      labelBackgroundColor = oldStyle.labelBackgroundColor
    )

    val oldVertexDesc = theory.vertexTypes(node)
    val newVertexDesc = new VertexDesc(oldVertexDesc.value,
      newStyle,
      oldVertexDesc.defaultData)

    val newVertexTypes: Map[String, VertexDesc] = theory.vertexTypes + (node -> newVertexDesc)
    // Update the base document's theory
    theory = new Theory(name = theory.name,
      coreName = theory.coreName,
      vertexTypes = newVertexTypes,
      edgeTypes = theory.edgeTypes,
      defaultVertexType = theory.defaultVertexType,
      defaultEdgeType = theory.defaultEdgeType
    )
  }

  def theory: Theory = document.theory

  def theory_=(th: Theory) = document.theory_=(th)

  // Prompts the user to select a type of node, including "custom" as an option
  def chooseNodeShape(node: String, current: (VertexShape, Option[Shape])): Unit = {
    val currentPath = current._2 match {
      case None => None
      case Some(shape) => Some(SVGUtils.shapeToPath(shape))
    }
    val dialog = new NodeShapeDialog(node, current._1, currentPath.getOrElse(""))
    dialog.centerOnScreen()
    dialog.open()
    val newShapeName: String = dialog.ShapeComboBox.item
    val newShape = VertexShape.fromName(newShapeName).getOrElse(VertexShape.Circle)
    val oldStyle: VertexStyleDesc = theory.vertexTypes(node).style
    val newStyle: VertexStyleDesc = new VertexStyleDesc(
      newShape,
      newShape match {
        case VertexShape.Custom =>
          try {
            Some(SVGUtils.pathToShape(dialog.CustomText.text))
          } catch {
            case e: Exception => new TheoryLoadException("Could not interpret custom shape path", e)
              None
          }
        case _ => None
      },
      strokeColor = oldStyle.strokeColor,
      fillColor = oldStyle.fillColor,
      labelPosition = oldStyle.labelPosition,
      labelForegroundColor = oldStyle.labelForegroundColor,
      labelBackgroundColor = oldStyle.labelBackgroundColor
    )

    val oldVertexDesc = theory.vertexTypes(node)
    val newVertexDesc = new VertexDesc(oldVertexDesc.value,
      newStyle,
      oldVertexDesc.defaultData)

    val newVertexTypes: Map[String, VertexDesc] = theory.vertexTypes + (node -> newVertexDesc)
    // Update the base document's theory
    theory = new Theory(name = theory.name,
      coreName = theory.coreName,
      vertexTypes = newVertexTypes,
      edgeTypes = theory.edgeTypes,
      defaultVertexType = theory.defaultVertexType,
      defaultEdgeType = theory.defaultEdgeType
    )
  }

  def createPageComponents(): Unit = {

    val separation = UserOptions.scaleInt(20)

    def horizontalWrap(component: Component): Unit = {
      EditorsCombined.contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), component, Swing.HStrut(10))
      }
    }

    val buttonSize = new Dimension(maxGridSize.width, scaleInt(30))

    EditorsCombined.contents.clear

    EditorsCombined.contents += Swing.VStrut(separation)
    horizontalWrap(new Label(
      """All changes are saved immediately. Re-open graphs to see the changes."""))
    horizontalWrap(new Label(
      """It is recommended that you back up your project before altering anything on this page!"""))

    EditorsCombined.contents += Swing.VStrut(separation)
    EditorsCombined.contents += new Separator()
    EditorsCombined.contents += Swing.VStrut(separation)

    horizontalWrap(new Label("Vertices:"))
    theory.vertexTypes.toSeq.sortBy(vt => vt._1).map(vt => {
      EditorsCombined.contents += new NodeEditor(vt._1, vt._2)
      EditorsCombined.contents += Swing.VStrut(separation)
    })
    val AddVertexButton = new Button("Add Vertex Type")
    AddVertexButton.preferredSize = buttonSize
    horizontalWrap(AddVertexButton)
    EditorsCombined.contents += Swing.VStrut(separation)
    EditorsCombined.contents += new Separator()
    EditorsCombined.contents += Swing.VStrut(separation)
    horizontalWrap(new Label("Edges:"))
    theory.edgeTypes.toSeq.sortBy(et => et._1).map(et => {
      EditorsCombined.contents += new EdgeEditor(et._1, et._2)
      EditorsCombined.contents += Swing.VStrut(separation)
    }
    )
    val AddEdgeButton = new Button("Add Edge Type")
    AddEdgeButton.preferredSize = buttonSize
    horizontalWrap(AddEdgeButton)
    EditorsCombined.contents += Swing.VStrut(separation)

    listenTo(AddEdgeButton, AddVertexButton)
    reactions += {
      case ButtonClicked(AddEdgeButton) =>
        addNewEdge()
      case ButtonClicked(AddVertexButton) =>
        addNewVertex()

    }
  }

  def addNewVertex(): Unit = {
    val d = new ChooseStringDialog("Name for new vertex type")
    d.centerOnScreen()
    d.open()

    val result = d.StringField.text
    if (result != "" && !theory.vertexTypes.keys.exists(k => k == result)) {
      val newVertexDesc = new VertexDesc(
        value = new ValueDesc(),
        style = new VertexStyleDesc(shape = VertexShape.Circle),
        defaultData = JsonObject(
          "type" -> Json.stringToJson(result),
          "value" -> Json.stringToJson("")
        )
      )
      val newVertexTypes: Map[String, Theory.VertexDesc] = theory.vertexTypes ++ Map(result -> newVertexDesc)
      // Update the base document's theory
      theory = new Theory(name = theory.name,
        coreName = theory.coreName,
        vertexTypes = newVertexTypes,
        edgeTypes = theory.edgeTypes,
        defaultVertexType = theory.defaultVertexType,
        defaultEdgeType = theory.defaultEdgeType
      )
    }
  }

  def addNewEdge(): Unit = {
    val d = new ChooseStringDialog("Name for new edge type")
    d.centerOnScreen()
    d.open()

    val result = d.StringField.text
    if (result != "") {
      if (!theory.edgeTypes.keys.exists(k => k == result)) {
        val newEdgeTypes: Map[String, Theory.EdgeDesc] = theory.edgeTypes ++ Map(result -> Theory.PlainEdgeDesc)
        // Update the base document's theory
        theory = new Theory(name = theory.name,
          coreName = theory.coreName,
          vertexTypes = theory.vertexTypes,
          edgeTypes = newEdgeTypes,
          defaultVertexType = theory.defaultVertexType,
          defaultEdgeType = theory.defaultEdgeType
        )
      } else {
        UserAlerts.alert("That name is already in use", Elevation.WARNING)
      }
    }
  }

  def colourToString(colour: Color): String = {
    var returnColour = f"#${0xFFFFFF & colour.hashCode()}%06X"
    for ((colourName, colourValue) <- approvedColours) {
      if (colourValue == colour) {
        returnColour = colourName
      }
    }
    returnColour
  }

  class ChooseStringDialog(title: String) extends Dialog {
    modal = true


    val AcceptButton = new Button("Accept")
    val CancelButton = new Button("Cancel")
    val StringField = new TextField("")
    defaultButton = Some(AcceptButton)
    val ShapeEditorPanel : BoxPanel = new BoxPanel(Orientation.Vertical) {

      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Name:"), Swing.HStrut(5), StringField, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), AcceptButton, Swing.HStrut(5), CancelButton, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
    }

    contents = ShapeEditorPanel

    listenTo(AcceptButton, CancelButton)

    reactions += {
      case ButtonClicked(AcceptButton) =>
        close()
      case ButtonClicked(CancelButton) =>
        StringField.text = ""
        close()
    }
  }

  class EdgeWidthDialog(name: String, current: Int) extends Dialog {
    title = s"Choose the stroke width for edge $name"
    modal = true

    val sizeOptions: Seq[String] = (1 to 5).map(_.toString)


    val AcceptButton = new Button("Accept")
    val CancelButton = new Button("Cancel")
    val PlacementComboBox = new ComboBox(sizeOptions)
    PlacementComboBox.selection.item = current.toString
    defaultButton = Some(CancelButton)
    val ShapeEditorPanel : BoxPanel = new BoxPanel(Orientation.Vertical) {

      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Size:"), Swing.HStrut(5), PlacementComboBox, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), AcceptButton, Swing.HStrut(5), CancelButton, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
    }

    contents = ShapeEditorPanel

    listenTo(AcceptButton, CancelButton, PlacementComboBox.selection)

    reactions += {
      case ButtonClicked(AcceptButton) =>
        close()
      case ButtonClicked(CancelButton) =>
        PlacementComboBox.item = current.toString
        close()
      case SelectionChanged(PlacementComboBox) =>
    }
  }

  class NodeLabelPlacementDialog(name: String, current: VertexLabelPosition) extends Dialog {
    title = s"Choose the label placement for node $name"
    modal = true

    val positionOptions: Seq[String] = VertexLabelPosition.values.
      map(vt => vt.toString).filterNot(s => s == "custom").toSeq


    val AcceptButton = new Button("Accept")
    val CancelButton = new Button("Cancel")
    val PlacementComboBox = new ComboBox(positionOptions)
    PlacementComboBox.selection.item = current.toString
    defaultButton = Some(CancelButton)
    val ShapeEditorPanel : BoxPanel = new BoxPanel(Orientation.Vertical) {

      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Label placement:"), Swing.HStrut(5), PlacementComboBox, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), AcceptButton, Swing.HStrut(5), CancelButton, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
    }

    contents = ShapeEditorPanel

    listenTo(AcceptButton, CancelButton, PlacementComboBox.selection)

    reactions += {
      case ButtonClicked(AcceptButton) =>
        close()
      case ButtonClicked(CancelButton) =>
        PlacementComboBox.item = current.toString
        close()
      case SelectionChanged(PlacementComboBox) =>
    }
  }

  class DataTypePickingDialog(str: String, current: String) extends Dialog {
    modal = true

    private var currentForBox : String = current

    private val currentParsedType : Option[ValueType] = {
      try {
        val types = CompositeExpression.parseTypes(current)
        if(types.length == 1) {
          currentForBox = ""
          Some(types.head)
        } else {
          currentForBox = valueTypeVectorToString(types)
          None
        }
      }
        catch {
          case GenericParseException(msg) => {
            currentForBox = ""
            None
          }
        }
    }

    val dataTypeOptions: Seq[String] = approvedDataTypes.values.toSet.toSeq.sorted :+ "composite"
    val AcceptButton = new Button("Accept")
    val CancelButton = new Button("Cancel")
    val DataComboBox = new ComboBox(dataTypeOptions)
    DataComboBox.selection.item = {
      if (currentParsedType.nonEmpty) {
        current
      } else {
        "composite"
      }
    }
    val CustomText = new TextField()
    CustomText.text = currentForBox
    val ShapeEditorPanel: BoxPanel = new BoxPanel(Orientation.Vertical) {

      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Data Type:"), Swing.HStrut(5), DataComboBox, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Composite Type:"), Swing.HStrut(5), CustomText, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), AcceptButton, Swing.HStrut(5), CancelButton, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
    }

    enableCustom()
    defaultButton = Some(CancelButton)


    def enableCustom(): Unit = {
      DataComboBox.selection.item match {
        case "composite" => CustomText.enabled = true
        case _ => CustomText.enabled = false
      }
    }

    contents = ShapeEditorPanel

    listenTo(AcceptButton, CancelButton, DataComboBox.selection)

    var wasAccepted : Boolean = false

    reactions += {
      case ButtonClicked(AcceptButton) =>
        wasAccepted = true
        close()
      case ButtonClicked(CancelButton) =>
        wasAccepted = false
        close()
      case SelectionChanged(DataComboBox) =>
        enableCustom()
    }
  }

  // example: ColourPickingDialog("X", "foreground colour", Color(10,10,200))
  class ColourPickingDialog(title: String, current: Color) extends Dialog {
    modal = true

    val currentColourName = colourToString(current)

    val colourOptions: Seq[String] = approvedColours.keys.toSet.toSeq.sorted :+ "custom"
    val AcceptButton = new Button("Accept")
    val CancelButton = new Button("Cancel")
    val ColourComboBox = new ComboBox(colourOptions)
    ColourComboBox.selection.item = currentColourName
    val CustomText = new TextField()
    displayColour(current)
    val ShapeEditorPanel: BoxPanel = new BoxPanel(Orientation.Vertical) {

      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Colour:"), Swing.HStrut(5), ColourComboBox, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Custom data:"), Swing.HStrut(5), CustomText, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), AcceptButton, Swing.HStrut(5), CancelButton, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
    }

    enableCustom()
    defaultButton = Some(CancelButton)

    def displayColour(colour: Color): Unit = {
      CustomText.text = f"#${0xFFFFFF & colour.hashCode()}%06X"
    }

    def enableCustom(): Unit = {
      ColourComboBox.selection.item match {
        case "custom" => CustomText.enabled = true
        case _ => CustomText.enabled = false
      }
    }

    contents = ShapeEditorPanel

    listenTo(AcceptButton, CancelButton, ColourComboBox.selection)

    reactions += {
      case ButtonClicked(AcceptButton) =>
        close()
      case ButtonClicked(CancelButton) =>
        ColourComboBox.item = currentColourName
        close()
      case SelectionChanged(ColourComboBox) =>
        ColourComboBox.selection.item match {
          case "custom" =>
          case s => displayColour(approvedColours(s))
        }
        enableCustom()
    }
  }

  class NodeShapeDialog(name: String, current: VertexShape, currentCustom: String) extends Dialog {
    title = s"Choose the shape for node $name"
    modal = true

    val shapeOptions: Seq[String] = VertexShape.values.
      map(vt => vt.toString).filterNot(s => s == "custom").toSeq


    val AcceptButton = new Button("Accept")
    val CancelButton = new Button("Cancel")
    val ShapeComboBox = new ComboBox(shapeOptions)
    ShapeComboBox.selection.item = current.toString
    val CustomText = new TextField(currentCustom)
    enableCustom()
    defaultButton = Some(CancelButton)
    val ShapeEditorPanel = new BoxPanel(Orientation.Vertical) {

      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Vertex shape:"), Swing.HStrut(5), ShapeComboBox, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), new Label("Custom data:"), Swing.HStrut(5), CustomText, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += (Swing.HStrut(10), AcceptButton, Swing.HStrut(5), CancelButton, Swing.HStrut(10))
      }
      contents += Swing.VStrut(10)
    }

    def enableCustom(): Unit = {
      ShapeComboBox.selection.item match {
        case "custom" => CustomText.enabled = true
        case _ => CustomText.enabled = false
      }
    }

    contents = ShapeEditorPanel

    listenTo(AcceptButton, CancelButton, ShapeComboBox.selection)

    reactions += {
      case ButtonClicked(AcceptButton) =>
        close()
      case ButtonClicked(CancelButton) =>
        ShapeComboBox.item = current.toString
        close()
      case SelectionChanged(ShapeComboBox) =>
        enableCustom()
    }
  }

  class NodeEditor(nodeName: String, desc: VertexDesc) extends BoxPanel(Orientation.Vertical) {
    // Note that GridPanel expands to fill parent
    maximumSize = maxGridSize
    contents += new GridPanel(4, 2) {

      contents += new Label("Name")
      contents += new Label(nodeName) // Currently doesn't support renaming nodes (and shouldn't?)

      contents += new Label("Shape")
      val EditShapeButton: Button = new Button(desc.style.shape.toString)
      contents += EditShapeButton

      contents += new Label("Colour")
      val EditColourButton: Button = new Button(colourToString(desc.style.fillColor))
      contents += EditColourButton

      contents += new Label("Values")
      val EditValueTypeButton: Button = new Button(valueTypeVectorToString(desc.value.typ))
      contents += EditValueTypeButton
      //contents += new Label("Label placement")
      //val EditPlacementButton: Button = new Button(desc.style.labelPosition.toString)
      //contents += EditPlacementButton
      //contents += new Label("Example")
      //contents += new Label("Example here")
      listenTo(EditShapeButton, EditColourButton, EditValueTypeButton)
      reactions += {
        case ButtonClicked(EditShapeButton) =>
          chooseNodeShape(nodeName, (desc.style.shape, desc.style.customShape))
        case ButtonClicked(EditColourButton) =>
          chooseNodeColour(nodeName, desc.style.fillColor)
        case ButtonClicked(EditValueTypeButton) =>
          chooseNodeDataType(nodeName, valueTypeVectorToString(desc.value.typ))
      }
    }
  }
val maxGridSize = new Dimension(scaleInt(200), scaleInt(100))

  class EdgeEditor(edgeName: String, desc: EdgeDesc) extends BoxPanel(Orientation.Vertical) {
    // Note that GridPanel expands to fill parent
    maximumSize = maxGridSize
    contents += new GridPanel(3, 2) {
      contents += new Label("Name")
      contents += new Label(edgeName) // Currently doesn't support renaming nodes (and shouldn't?)
      contents += new Label("Width")
      val EditShapeButton: Button = new Button(desc.style.strokeWidth.toString)
      contents += EditShapeButton
      contents += new Label("Colour")
      val EditColourButton: Button = new Button(colourToString(desc.style.strokeColor))
      contents += EditColourButton
      //contents += new Label("Label placement")
      //val EditPlacementButton: Button = new Button(desc.style.labelPosition.toString)
      //contents += EditPlacementButton
      //contents += new Label("Example")
      //contents += new Label("Example here")
      listenTo(EditShapeButton, EditColourButton)
      reactions += {
        case ButtonClicked(EditShapeButton) =>
          chooseEdgeWidth(edgeName, desc.style.strokeWidth)
        case ButtonClicked(EditColourButton) =>
          chooseEdgeColour(edgeName, desc.style.strokeColor)
        //case ButtonClicked(EditPlacementButton) =>
        //  chooseLabelPlacement(name, desc.style.labelPosition)
      }
    }
  }

  createPageComponents()


  add(TopScrollablePane, BorderPanel.Position.Center)

  add(Toolbar, BorderPanel.Position.North)

  listenTo(document)

  reactions += {
    case TheoryChanged() =>
      alert("Theory changed", Elevation.DEBUG)
      createPageComponents()

  }
}
