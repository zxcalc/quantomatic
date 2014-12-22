/*  Title:      Pure/Isar/keyword.scala
    Author:     Makarius

Isar command keyword classification and keyword tables.
*/

package isabelle


object Keyword
{
  /* kinds */

  val MINOR = "minor"
  val CONTROL = "control"
  val DIAG = "diag"
  val THY_BEGIN = "thy_begin"
  val THY_END = "thy_end"
  val THY_HEADING1 = "thy_heading1"
  val THY_HEADING2 = "thy_heading2"
  val THY_HEADING3 = "thy_heading3"
  val THY_HEADING4 = "thy_heading4"
  val THY_DECL = "thy_decl"
  val THY_LOAD = "thy_load"
  val THY_GOAL = "thy_goal"
  val QED = "qed"
  val QED_SCRIPT = "qed_script"
  val QED_BLOCK = "qed_block"
  val QED_GLOBAL = "qed_global"
  val PRF_HEADING2 = "prf_heading2"
  val PRF_HEADING3 = "prf_heading3"
  val PRF_HEADING4 = "prf_heading4"
  val PRF_GOAL = "prf_goal"
  val PRF_BLOCK = "prf_block"
  val PRF_OPEN = "prf_open"
  val PRF_CLOSE = "prf_close"
  val PRF_CHAIN = "prf_chain"
  val PRF_DECL = "prf_decl"
  val PRF_ASM = "prf_asm"
  val PRF_ASM_GOAL = "prf_asm_goal"
  val PRF_ASM_GOAL_SCRIPT = "prf_asm_goal_script"
  val PRF_SCRIPT = "prf_script"


  /* categories */

  val diag = Set(DIAG)
  val control = Set(CONTROL)

  val heading = Set(THY_HEADING1, THY_HEADING2, THY_HEADING3, THY_HEADING4,
    PRF_HEADING2, PRF_HEADING3, PRF_HEADING4)

  val theory =
    Set(THY_BEGIN, THY_END, THY_HEADING1, THY_HEADING2, THY_HEADING3, THY_HEADING4,
      THY_LOAD, THY_DECL, THY_GOAL)

  val theory_body =
    Set(THY_HEADING1, THY_HEADING2, THY_HEADING3, THY_HEADING4, THY_LOAD, THY_DECL, THY_GOAL)

  val proof =
    Set(QED, QED_SCRIPT, QED_BLOCK, QED_GLOBAL, PRF_HEADING2, PRF_HEADING3, PRF_HEADING4,
      PRF_GOAL, PRF_BLOCK, PRF_OPEN, PRF_CLOSE, PRF_CHAIN, PRF_DECL,
      PRF_ASM, PRF_ASM_GOAL, PRF_ASM_GOAL_SCRIPT, PRF_SCRIPT)

  val proof_body =
    Set(DIAG, PRF_HEADING2, PRF_HEADING3, PRF_HEADING4, PRF_BLOCK, PRF_OPEN, PRF_CLOSE, PRF_CHAIN,
      PRF_DECL, PRF_ASM, PRF_ASM_GOAL, PRF_ASM_GOAL_SCRIPT, PRF_SCRIPT)

  val theory_goal = Set(THY_GOAL)
  val proof_goal = Set(PRF_GOAL, PRF_ASM_GOAL, PRF_ASM_GOAL_SCRIPT)
  val qed = Set(QED, QED_SCRIPT, QED_BLOCK)
  val qed_global = Set(QED_GLOBAL)
}

