package quanto.layout

import JaCoP.constraints._
import JaCoP.core._
import JaCoP.search._

// based heavily on the scala wrappers found in JaCoP.scala

trait ConstraintSolver {
  var timeOutValue = 0
  private val store = new Store
  val constraints = new collection.mutable.ListBuffer[Constraint]
  val MinInt = IntDomain.MinInt
  val MaxInt = IntDomain.MaxInt

  private def imposeAllConstraints() {
    constraints.foreach(e => store.impose(e))
    constraints.clear()
  }

  private var freshVar = 0

  protected class IntVar(name: String, min: Int, max: Int)
    extends JaCoP.core.IntVar(store, name, min, max) {
    def this(min: Int, max: Int) = {
      this("_$" + freshVar, min, max)
      freshVar += 1
    }

    def this(name: String) = this(name, MinInt, MaxInt)
    def this() = this(MinInt, MaxInt)

    def +(that: JaCoP.core.IntVar) = {
      val fresh = new IntVar()
      constraints += new XplusYeqZ(this, that, fresh)
      fresh
    }

    def +(that: Int) = {
      val fresh = new IntVar()
      constraints += new XplusCeqZ(this, that, fresh)
      fresh
    }

    def -(that: JaCoP.core.IntVar) = {
      val fresh = new IntVar()
      constraints += new XplusYeqZ(fresh, that, this)
      fresh
    }

    def -(that: Int) = {
      val fresh = new IntVar()
      constraints += new XplusCeqZ(fresh, that, this)
      fresh
    }

    def *(that: JaCoP.core.IntVar) = {
      val fresh = new IntVar()
      constraints += new XmulYeqZ(this, that, fresh)
      fresh
    }

    def *(that: Int) = {
      val fresh = new IntVar()
      constraints += new XmulCeqZ(this, that, fresh)
      fresh
    }

    def div(that: JaCoP.core.IntVar) = {
      val fresh = new IntVar()
      constraints += new XdivYeqZ(this, that, fresh)
      fresh
    }

    def mod(that: JaCoP.core.IntVar) = {
      val fresh = new IntVar()
      constraints += new XmodYeqZ(this, that, fresh)
      fresh
    }

    def ^(that: JaCoP.core.IntVar) = {
      val fresh = new IntVar()
      constraints += new XexpYeqZ(this, that, fresh)
      fresh
    }

    def unary_- = {
      val fresh = new IntVar()
      constraints += new XplusYeqC(this, fresh, 0)
      fresh
    }

    def #=(that: JaCoP.core.IntVar) = new XeqY(this, that)
    def #=(that: Int) = new XeqC(this, that)
    def #!=(that: JaCoP.core.IntVar) = new XneqY(this, that)
    def #!=(that: Int) = new XneqC(this, that)
    def #<(that: JaCoP.core.IntVar) = new XltY(this, that)
    def #<(that: Int) = new XltC(this, that)
    def #<=(that: JaCoP.core.IntVar) = new XlteqY(this, that)
    def #<=(that: Int) = new XlteqC(this, that)
    def #>(that: JaCoP.core.IntVar) = new XgtY(this, that)
    def #>(that: Int) = new XgtC(this, that)
    def #>=(that: JaCoP.core.IntVar) = new XgteqY(this, that)
    def #>=(that: Int) = new XgteqC(this, that)
  }

  protected object IntVar {
    def apply(name: String, min: Int, max: Int) = new IntVar(name, min, max)
    def apply(name: String) = new IntVar(name)
    def apply(min: Int, max: Int) = new IntVar(min, max)
    def apply() = new IntVar()
  }


  protected def satisfy(vars: Array[IntVar]): Boolean = {
    imposeAllConstraints()
    val label = new DepthFirstSearch[IntVar]
    val select = new SimpleSelect(vars, new SmallestDomain, new IndomainMin)
    label.setPrintInfo(false)

    if (timeOutValue > 0) label.setTimeOut(timeOutValue)
    label.labeling(store, select)
  }

  protected def minimize(vars: Array[IntVar], cost: IntVar): Boolean = {
    imposeAllConstraints()
    val label = new DepthFirstSearch[IntVar]
    val select = new SimpleSelect(vars, new SmallestDomain, new IndomainMin)
    label.setPrintInfo(false)

    if (timeOutValue > 0) label.setTimeOut(timeOutValue)
    label.labeling(store, select, cost)
  }
}
