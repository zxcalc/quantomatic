package quanto.gui

import swing._
import swing.event._
import quanto.util.{SockJson,SockJsonError,SockJsonErrorType}


class EvalController (
  ConnectButton : Button,
  DisconnectButton : Button,
  BacktrackButton : Button,
  NextButton : Button,
  PrevButton : Button,
  errorDlg : String => Unit,
  graphDocument : GraphDocument ){

  var prev_counts = 0 /* apply backtracking only to the latest status, keep record of applying prev */

  def setEvalButtonStatus (con : Boolean, discon : Boolean, back : Boolean, prev : Boolean, next : Boolean) {
    BacktrackButton.enabled_=(back);
    PrevButton.enabled_=(prev);
    NextButton.enabled_=(next);
    ConnectButton.enabled_=(con);
    DisconnectButton.enabled_=(discon);
  }

  val _ = setEvalButtonStatus (true, false, false, false, false);

  private def error(action: String, reason: String) {
    val message = action + " (" + reason + ") "
    errorDlg (message)
  }

  // add listeners and even handlers
  ConnectButton.listenTo(ConnectButton.mouse.clicks)
  ConnectButton.reactions += {
    case MouseClicked (_,_,_,_,_)=>
      try{
        SockJson.connectSock();
        try{
          val mode = SockJson.requestMode ();
          /* TOOD: LYH - need to check that if the graph is valid, e.g. containing GN ? */
          val edata = SockJson.requestInit(mode, graphDocument.exportJson());
          graphDocument.loadGraph (edata);
          graphDocument.reLayout();
          setEvalButtonStatus (false, true, false, false, true)
          prev_counts = 0;
        }
        catch{
          case _ => error ("Can't init graph", "graph can't be initialised")
        }
      }
      catch{
        case _ => error ("Can't connect to Isabelle", "socket err")
      }
  }

  DisconnectButton.listenTo(DisconnectButton.mouse.clicks)
  DisconnectButton.reactions += {
    case MouseClicked (_,_,_,_,_) =>
      SockJson.requestDeinit ();
      SockJson.closeSock ();
      setEvalButtonStatus (true, false, false, false, false);

  }

  BacktrackButton.listenTo(BacktrackButton.mouse.clicks)
  BacktrackButton.reactions += {
    case MouseClicked (_,_,_,_,_) =>
      try{
        val edata = SockJson.requestBacktrack();
        graphDocument.loadGraph (edata);
        graphDocument.reLayout();
        setEvalButtonStatus (false, true, true, true, true);
      }
      catch{
        case SockJsonError(_, SockJsonErrorType.ErrEval) =>
          setEvalButtonStatus (false, true, false, false, false);
          error ("Can't show the current step", "eval error")
        case SockJsonError(_, SockJsonErrorType.NoBacktrack) =>
          BacktrackButton.enabled_=(false)
          error ("No backtracking is available", "No braching")

      }
  }

  PrevButton.listenTo(PrevButton.mouse.clicks)
  PrevButton.reactions += {
    case MouseClicked (_,_,_,_,_) =>
      try{
        val edata = SockJson.requestPrev();
        graphDocument.loadGraph (edata);
        graphDocument.reLayout();
        setEvalButtonStatus (false, true, false, true, true);
        prev_counts = prev_counts + 1;
      }
      catch{
        case SockJsonError(_, SockJsonErrorType.ErrEval) =>
          setEvalButtonStatus (false, true, false, false, false);
          error ("Can't show the current step", "eval error")

        case SockJsonError(_, SockJsonErrorType.NoPrev) =>
          PrevButton.enabled_=(false)
          error ("No prev step", "already the first step now")

        case _ =>
          error ("Can't show the previous step", "unknown error")
      }
  }

  NextButton.listenTo(NextButton.mouse.clicks)
  NextButton.reactions += {
    case MouseClicked (_,_,_,_,_) =>
      try{
        val edata = SockJson.requestNext();
        graphDocument.loadGraph (edata);
        graphDocument.reLayout();
        if (prev_counts > 0)
          prev_counts = prev_counts - 1;
        setEvalButtonStatus (false, true, (prev_counts == 0), true, true);

      }
      catch{
        case SockJsonError(_, SockJsonErrorType.ErrEval) =>
          setEvalButtonStatus (false, true, false, false, false);
          error ("Can't show the current step", "eval error")

        case SockJsonError(_, SockJsonErrorType.GoodEval) =>
          NextButton.enabled_=(false)
          error ("Congrats", "proof strategy language succeeds !")

        case _ =>
          error ("Can't eval the next step", "unknown error")
      }
  }
    /* end of Eval events */

}