quantomatic can be operated directly from the ML toplevel.  If you are
starting quanto using the HOL_IsaP heap, do:

ML> use "ROOT.ML";

Lots of garabage will fly up the screen; now do :

ML> Controller.init () ;

(Alternatively start polyML with the quanto heap and miss out the above)

Quantomatic is now awaiting your command.  The first thing to do is
type "M" then press return to get it into interactive mode.  You will
see:

Changed output mode.
quanto:> 

Now you can type commands.  The commands are all written one per line;
pressing return ends the line and tells quanto to process your
command.  

Commands are CASE SENSITIVE.  If you type garbage in you get a message
"Illegal command: whatyoutyped".  You can try again.  Anything following a legal
command on the same line is ignored.  Certain commands will accept
bad data and then let quanto crash.  I have flagged the places where
this can happen, as far as I know.

The commands are:

Q --> quit

H --> print welcome message

M --> change mode (the choices are interactive and piped; piped is the
default, but interactive is easier for humans to read)

D --> dump current graph to terminal (in XML by default)

n --> start a new graph

r --> add a red vertex to the graph

g --> add a green vertex to the graph

h --> add a H vertex to the graph

b --> add a boundary vertex to the graph

e vertex1 vertex2 --> add an edge between vertex1 and vertex2.  You
should replace "vertex1" and "vertex2" with the names of actual
vertices in the graph (look at the dumped output to see them) If they
are not correct quanto will crash.

d vertex --> removes "vertex" from the graph; if "vertex" is not a
vertex in the graph, quanto will crash.

up vertex bound colour --> updates the data of "vertex" (if vertex
does not occur in the graph -- you guessed it! -- quanto will crash).
bound should be either "true" or "false".  Anything else is will be
rejected (though quanto WON'T crash). If bound is "false" then you must
also supply a colour which must be either "red", "green" or "h";
anything else will be rejected.  It is not currently possible to
update the angle expressions.  If bound is "true" then the colour is
ignored.

u --> undo the last command;  there is no redo.

At present it's not possible to switch between XML and dot output from
the quanto prompt.  The ML function "Controller.switch_mode ();" wil
do it for you.

I guess it's possible to put these commands into a file and drive
quanto that way.