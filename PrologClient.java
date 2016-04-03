package Glomus.Client.Prolog;

import Glomus.Client.Java.*;
import java.util.*;

/**
 * A client that wraps a player implemented in Prolog. 
 *
 * This program should require no modification since it simply provides 
 * the interface between the Server and the SWI-Prolog engine executing 
 * the Prolog client.
 *
 * @author David M. Hansen
 * @version 1.2
 *    1.2 modified all jpl. to jpl.org.jpl7.
 *
 * @see Glomus.Client.Java.Client
 * @see Glomus.Client.Java.JavaClient
 * @see Glomus.Client.Java.ServerConnection
 * @see jpl 
 */
public class PrologClient implements Client
{

   /**
    * Constructor set the source file that init will use
    *
    * @param prologSourceFile filename of prolog code to use
    */
   public PrologClient(String prologSourceFile)
   {
      p_prologSourceFile = prologSourceFile;
   }

   /**
    * Initiates the SWI-Prolog connection, "consults" the Prolog client code
    * (from <code>PrologClient.pl</code>) and initialize the player's 
    * knowledgebase with the suspects, weapons, rooms (and their connectivity), 
    * and the cards that this player has.
    *
    * @param identity Our name
    * @param startRoom Our starting location
    * @param suspects The set of all suspect names
    * @param weapons The set of all weapon names
    * @param rooms The map of all rooms to a list of adjacent rooms
    * @param cards The set of all cards this player has
    * @param players A list of suspects who are active in the game, in
    * order of play (if that's important)
    */
   public void init(String identity, String startRoom,
                HashSet<String> suspects, HashSet<String> weapons,
                HashMap<String, HashSet<String>> rooms,
                HashSet<String> cards, ArrayList<String> players)
   {
      int termNumber; // To keep track of which term we're adding to a list
      jpl.org.jpl7.Term[] roomLists; // For building lists of adjacent rooms

      // The first time we're called, we need to inialize the Prolog
      // player
      if (!p_prologInitialized)
      {
         // Set the JPL wrapper to invoke prolog without any signal
         // handling so that serious errors are passed along to this
         // program
         jpl.org.jpl7.JPL.setDefaultInitArgs(new String[] {"pl", "-g", "true", "-nosignals"}); 
////???         jpl.org.jpl7.JPL.init(new String[] {"swipl", "-g", "true", "--nosignals"}); 
         // Initialize the connection to Prolog and consult the Prolog 
         // source file
         queryProlog(!RETURN_AN_ANSWER, "consult", p_prologSourceFile);

         p_prologInitialized = true;

         // Populate our maps from symbols to Prolog symbols and
         // vice-versa. Replace blanks with underscore and convert to all
         // lowercase as required for Prolog atoms. 
         for (String suspect : suspects)
         {
            // Remove any non-letters and digits in the string 
            // string to avoid most collisions with Prolog special
            // characters such as capital letters for variables, '.',
            // and leading '_'. The value we use here is really
            // irrelevant anyway and could be just an arbitrary string.
            p_strToPrologSymbol.put(suspect, suspect.toLowerCase().replaceAll("[^a-z0-9]",""));
          
            p_prologSymbolToStr.put(suspect.toLowerCase().replaceAll("[^a-z0-9]",""), suspect);
         }
         for (String weapon : weapons)
         {
            p_strToPrologSymbol.put(weapon, weapon.toLowerCase().replaceAll("[^a-z0-9]",""));
            p_prologSymbolToStr.put(weapon.toLowerCase().replaceAll("[^a-z0-9]",""), weapon);
         }
         for (String room : rooms.keySet())
         {
            p_strToPrologSymbol.put(room, room.toLowerCase().replaceAll("[^a-z0-9]",""));
            p_prologSymbolToStr.put(room.toLowerCase().replaceAll("[^a-z0-9]",""), room);
         }
/*****
System.out.println(p_strToPrologSymbol);
System.out.println(p_prologSymbolToStr);
*****/
      } // if


      // Prepare to call the Prolog player's "init" predicate.
      // For each of the rooms, we need to construct a 2-element list
      // where the first element is the name of the room and the second
      // element is a list of adjacent rooms.
      roomLists= new jpl.org.jpl7.Term[rooms.size()];
      termNumber = 0;
      for (String roomName : rooms.keySet())
      {
         jpl.org.jpl7.Term[] roomList = {
            new jpl.org.jpl7.Atom(p_strToPrologSymbol.get(roomName)),
            termList(rooms.get(roomName))
         };
         roomLists[termNumber++] = jpl.org.jpl7.Util.termArrayToList(roomList);
      }


      // Create a query that consists of the name of the functor we wish
      // to call (init), followed by an array of Term objects, some of
      // which are Prolog lists created via the termList method. E.g.,
      // init(MissScarlet, BilliardRoom, [MissScarlet, MrGreen,...],
      //     [Rope, Candlestick, ...], 
      //     [[Hall,[Conservatory, BilliardRoom, ...]], ...],
      //     [Rope, MrGreen, ...],
      //     [MissScarlet, MrGreen, ...])
      jpl.org.jpl7.Query query = new jpl.org.jpl7.Query(
            new jpl.org.jpl7.Compound(
               "init",
                new jpl.org.jpl7.Term[] {
                     new jpl.org.jpl7.Atom(p_strToPrologSymbol.get(identity)),
                     new jpl.org.jpl7.Atom(p_strToPrologSymbol.get(startRoom)),
                     termList(suspects),
                     termList(weapons),
                     // Convert the array of 2-term lists we created for each room
                     // above into a JPL List (of lists)
                     jpl.org.jpl7.Util.termArrayToList(roomLists),
                     termList(cards),
                     termList(players)
                  }
            )
      );


      // Execute the init statement which had better succeed!
      if (!query.hasSolution())
         displayErrorAndExit("Query failed - "+query);

      // Close the query
      query.close();

   } // init



   /**
    * A suggestion overheard by all players
    *
    * @param suspect Who did it
    * @param weapon How they did it
    * @param room Where they did it
    * @param player The player making the accusation
    */
   public void suggestion(String suspect, String weapon, String room,
         String player)
   {
      // The the Prolog player what we've heard
      queryProlog(!RETURN_AN_ANSWER, "suggests", suspect, weapon, room, player);
   } // suggestion


   /**
    * A request for this player to make a suggestion
    *
    * @param server The Glomus Server to reply to
    */
   public void makeSuggestion(ServerConnection server)
   {
      // Make the suggestion as given by Prolog
      ArrayList<String> answer = queryProlog(RETURN_AN_ANSWER, "makeSuggestion", 
            "Suspect","Weapon","Room","Identity");

      // Get the values to return from the answer returned by the query
      server.sendSuggestion(answer.get(0), answer.get(1), 
            answer.get(2), answer.get(3));
   } // makeSuggestion


   /**
    * A request for this player to make an accusation, if we wish
    *
    * @param server The Glomus Server to reply to
    */
   public void makeAccusation(ServerConnection server)
   {
      // Make the accusation - if we're not ready, makeAccusation will
      // bind all variables to 'null' indicating no accusation
      ArrayList<String> answer = queryProlog(RETURN_AN_ANSWER, "makeAccusation", 
            "Suspect","Weapon","Room","Identity");
     
      // Send the answer back
      server.sendAccusation(answer.get(0), answer.get(1), 
            answer.get(2), answer.get(3));
   } // makeAccusation


   /**
    * A request for this player to refute the claim, if possible
    *
    * @param suspect Who did it
    * @param weapon How they did it
    * @param room Where they did it
    * @param player The player making the accusation
    * @param server The Glomus Server to reply to
    */
   public void refute(String suspect, String weapon, String room,
         String player, ServerConnection server)
   {
      // Attempt to refute. Our answer will be bound to the "Card"
      // variable at the end (or null if we can't refute).
      ArrayList<String> answer = queryProlog(RETURN_AN_ANSWER, "refute", 
            suspect, weapon, room, player, "Card");

      // Refute with the card we found. If null, that indicates we can't
      // refute
      server.refuteWith(answer.get(4));
   } // refute


   /**
    * A refuted suggestion overheard
    *
    * @param suspect Who did it
    * @param weapon How they did it
    * @param room Where they did it
    * @param player The player who refuted the suggestion
    */
   public void refuted(String suspect, String weapon, String room,
         String player)
   {
      // The the Prolog player what we've heard
      queryProlog(!RETURN_AN_ANSWER, "refutes", suspect, weapon, room, player);
   }


   /**
    * A failed refutation overheard
    *
    * @param suspect Who did it
    * @param weapon How they did it
    * @param room Where they did it
    * @param player is the player who could not refute the suggestion
    */
   public void cantRefute(String suspect, String weapon, String room,
         String player)
   {
      // The the Prolog player what we've heard
      queryProlog(!RETURN_AN_ANSWER, "cantRefute", suspect, weapon, room, player);
   }


   /**
    * A failed accusation overheard (successful ones end the game, and
    * thus do not need to be reported)
    *
    * @param suspect Who did it
    * @param weapon How they did it
    * @param room Where they did it
    * @param player The player who failed their accusation
    */
   public void accuse(String suspect, String weapon, String room,
         String player)
   {
      // The the Prolog player what we've heard
      queryProlog(!RETURN_AN_ANSWER, "accuse", suspect, weapon, room, player);
   }


   /**
    * A refutation of our suggestion from another player
    *
    * @param player The player who is refuting our suggestion
    * @param card A suspect, weapon, or room that they show to us,
    * refuting our claim
    */
   public void shows(String player, String card)
   {
      // The the Prolog player what we've heard
      queryProlog(!RETURN_AN_ANSWER, "showsMe", player, card);
   }


   /**
    * A request to determine which pieces of information in the case
    * folder this player knows
    *
    * @param server The Glomus Server to reply to
    */
   public void knows(ServerConnection server)
   {
      // Issue four queries to Prolog and see which values we know. If
      // we know the value, the Prolog symbol will be returned,
      // otherwise we'll get a null back.
      ArrayList<String> suspectAnswer = queryProlog(RETURN_AN_ANSWER, "isSuspect", "Suspect");
      ArrayList<String> weaponAnswer = queryProlog(RETURN_AN_ANSWER, "isWeapon", "Weapon");
      ArrayList<String> roomAnswer = queryProlog(RETURN_AN_ANSWER, "isRoom", "Room");
      ArrayList<String> identity = queryProlog(RETURN_AN_ANSWER, "iAm", "Me");

	   // Send what we know to the server
      server.sendKnown(suspectAnswer.get(0),weaponAnswer.get(0), roomAnswer.get(0), identity.get(0));
   }


   /**
    * Informs the player of what another player in the game knows for
    * certain
    *
    * @param suspect Who did it
    * @param weapon How they did it
    * @param room Where they did it
    * @param player The player who knows the given info
    */
   public void endKnowledge(String suspect, String weapon, String room,
         String player)
   {
      // Tell the AI player what a particular player knew
      queryProlog(!RETURN_AN_ANSWER, "knew", suspect, weapon, room, player);
   }



   /**
    * Tells the client who won and what the answer was
    *
    * @param suspect Who did it
    * @param weapon How they did it
    * @param room Where they did it
    * @param player The player who won the game
    */
   public void answer(String suspect, String weapon, String room,
         String player)
   {
      // Tell the AI player what the answer was and who won
      queryProlog(!RETURN_AN_ANSWER, "answer", suspect, weapon, room, player);
   }


   /**
    * Notifies the client of some user-specific information
    *
    * @param message The message to pass onto the user
    */
   public void message(String message)
   {
      // The client interface specifies that this method must be
      // implemented, though its use is exclusively for passing
      // information onto a user (such as score data), information
      // which an AI cares nothing for. (Though it should be noted
      // that if the JavaClient class is used to wrap an AI player
      // built by a user, the semantics of this method will change
      // and it should be used)
      System.out.println("Server Message:\n\t"+message);
   }


   /**
    * Query Prolog using the given set of arguments and return the
    * answer
    *
    * @param returnAnAnswer determines whether we just check for an
    *        answer or return the answer
    * @param predicate the name of the Prolog predicate to call
    * @param args a list of arguments and variables for the predicate
    * @returns list of args bound to variables; only variables (i.e.,
    * strings beginning with a capital letter) will potentially be bound
    * to a value, all others will be null
    */
   private ArrayList<String> queryProlog(boolean returnAnAnswer, 
         String predicate, String... args)
   {
      // List of strings to return - they will be mappings of the
      // arguments in the same order as the arguments in args
      ArrayList<String> toReturn = new ArrayList<String>();
      Map<String, jpl.org.jpl7.Term> answer;    // To hold answer from Prolog
      // An array of terms to fill with the args to be passed to Prolog
      jpl.org.jpl7.Term[] terms = new jpl.org.jpl7.Term[args.length];

      int termNumber = 0; // To keep track of which term we're constructing

      // For each of the arguments, construct a JPL term and add this to
      // the array of terms for the query.
      for (String argument : args)
      {
         // If we're not returning an answer (i.e., there are no
         // variables in the arguments) or if this argument is the name 
         // of a game symbol we saw during the initialization phase, then 
         // add it to the list as a term. Otherwise we'll add the string
         // as a JPL Variable which will eventually be bound to a return
         // value by our query.
         if (!returnAnAnswer || p_strToPrologSymbol.containsKey(argument))
         {
            // If this is the name of a game symbol, look up and use the
            // translation to the Prolog symbol
            if (p_strToPrologSymbol.containsKey(argument))
            {
               // Pass the Prolog symbol as a JPL Atom
               terms[termNumber] = new jpl.org.jpl7.Atom(p_strToPrologSymbol.get(argument));
            }
            else // Not a game symbol we've seen before
            {
               // Not a symbol, probably a literal (such as the name of the 
               // file to consult in a "consult" query) so pass it on as-is
               // by wrapping it up in a JPL Atom
               terms[termNumber] = new jpl.org.jpl7.Atom(argument);
            }
         }
         else // Otherwise it's a new variable in the query
         {
            terms[termNumber] = new jpl.org.jpl7.Variable(argument);
         }

         termNumber++;

      } // for

      // Query Prolog with the predicate and the list of terms we just
      // built.
      jpl.org.jpl7.Query query = new jpl.org.jpl7.Query(new jpl.org.jpl7.Compound(predicate, terms));

      // If we're NOT returning an answer, then check for a solution
      // (should be one) and return null
      if (!returnAnAnswer)
      {
         // Query better have a solution (i.e., it succeeded), otherwise 
         // display an error and quit
         if (!query.hasSolution()) {
            displayErrorAndExit("Query failed - "+query);
         }
         query.close(); // Close the query, we don't want the answer
         return null;
      }
      else // A single answer IS expected
      {
         // Get the answer and extract bound values for each of the parameters
         // to the original query
         answer = query.oneSolution();

         // Get the bindings for any of the variables that were part of
         // the argument string that have been bound to variables and
         // return them in the same order as the arguments. Note that
         // if the predicate fails, then answer will be null in which case
         // we simply set all results to 'null' values
         for (String argument : args)
         {
            // If Prolog returned a null value for the answer or 
            // if a particular argument is not bound in the answer,
            // return 'null' for that argument - this will take care of those 
            // parameters that we mapped to Atoms above (i.e., that are
            // not variables in the query); only parameters that were mapped 
            // to a Variable will have a value and that value should be one 
            // of the game symbols we saw during initialization.
            if (answer==null || !answer.containsKey(argument))
            {
               toReturn.add(null);
            }
            else // Otherwise look up the string for this game symbol 
            {
               // We must convert the JPL Term to it's string
               // represenatation and then look up the string.
               toReturn.add(p_prologSymbolToStr.get(answer.get(argument).toString()));
            }
         } // for

         return toReturn;

      } // else

   } // queryProlog



   /** 
    * Convert a collection of String into a Prolog JPL List as a Term
    *
    * @param strings the strings to put into the List
    * @return Term that is a Prolog List
    */
   private jpl.org.jpl7.Term termList(Collection<String> strings)
   {
      jpl.org.jpl7.Term[] termArray = new jpl.org.jpl7.Term[strings.size()];
      int termNumber = 0; // To keep track of which term we're adding

      // Iterate over the list of strings, build a JPL Atom for each,
      // and place the Atom into the list of JPL Terms
      for (String str : strings)
      {
         // Create a JPL Atom from this string and add it to the array of
         // terms - look up each string and map it to the appropriate
         // prolog symbol
         termArray[termNumber++] = new jpl.org.jpl7.Atom(p_strToPrologSymbol.get(str));
      }
      // Return the array as a Prolog List of Term
      return jpl.org.jpl7.Util.termArrayToList(termArray);
   }


   /**
    * A severe error occured, print the error message and quit
    *
    * @param errorMessage to display
    */
   private void displayErrorAndExit(String errorMessage)
   {
      System.err.println(errorMessage);
      System.exit(1);
   }



   /**
    * Connect to the server, initialize the game, and have the
    * server play with an instance of this class for the 'brains'
    *
    * @param args Parameters for setting up the game
    *
    * @throws CmdLineParser.OptionException for illegal parameters
    */
   public static void main(String[] args) throws java.io.IOException
   {
      final int NUM_GAMES = 100; // Number of games to play

      // Parse the input parameters and use them to create a new
      // ServerConnection object

      try
      {

         // In case the user provides a custom setup file, we'll need to
         // remember what they give us in the file
         String setupParameters = null;

         // Create a new command line parser and denote the options we'll
         // accept
         CmdLineParser parser = new CmdLineParser();
         CmdLineParser.Option optCreate = parser.addIntegerOption('c',"create");
         CmdLineParser.Option optGames = parser.addIntegerOption('g',"games");
         CmdLineParser.Option optSetup = parser.addStringOption('s',"setup");
         CmdLineParser.Option optPort = parser.addIntegerOption('p',"port");
         CmdLineParser.Option optLog = parser.addBooleanOption('l',"log");
         CmdLineParser.Option optTest = parser.addBooleanOption('t',"test");
         CmdLineParser.Option optOpponents = parser.addStringOption('o',"opponent");
         CmdLineParser.Option optName = parser.addStringOption('n',"name");

         // Parse the command line options.
         parser.parse(args);

         // If the player wants to create a new game they'll pass the
         // -create option with the number of players
         Integer numPlayers = (Integer)parser.getOptionValue(optCreate,0);
         Integer numGames = (Integer)parser.getOptionValue(optGames, NUM_GAMES);
         String setupFile = (String)parser.getOptionValue(optSetup);
         Integer port = (Integer)parser.getOptionValue(optPort, 
               ServerConnection.DEFAULT_PORT);
         Boolean testOnly = (Boolean)parser.getOptionValue(optTest, Boolean.FALSE);
         // If user requested logging, then set our logging flag.
         Boolean logging = (Boolean)parser.getOptionValue(optLog, Boolean.FALSE);
         String preferredName = (String)parser.getOptionValue(optName,"");
         Vector<String> opponents = (Vector<String>)parser.getOptionValues(optOpponents);

         // Get the remaining command-line arguments - there should be at
         // least two: the name of the game and the host to connect to
         String[] otherArgs = parser.getRemainingArgs();
         if (otherArgs.length < 2)
         {
            throw new CmdLineParser.UnknownOptionException("Insuffucient Parameters");
         }


         // Establish the connection with the Glomus Server, using
         // the command-line arguments to determine where the server is
         // and to pass information about us as a client to the server.
         // Pass a new instance of this class to the server as the player
         // that will play the game. The last two arguments are the name
         // of the game and the host machine to contact. Note that the
         // last argument should be the filename of the Prolog code to
         // use so we pass that to the PrologClient instance.
         ServerConnection glomusServer =
            new ServerConnection(new PrologClient(
                     (otherArgs.length>2?otherArgs[2]:DEFAULT_SOURCE)),
                  numPlayers, numGames, setupFile,
                  port, testOnly, logging, preferredName, 
                  opponents, otherArgs[0], otherArgs[1]); 

         // Let the server play away!
         glomusServer.play();
      }
      catch (CmdLineParser.OptionException e)
      {
         System.err.println(
            "Usage: java <client> [-options] game host [prolog-filename]\n\n"+
            "where options include:\n"+
            "  [{-c, --create} numPlayers]   create new game with numPlayers\n" +
            "  [{-n, --name}]                preferred name for your player\n" +
            "  [{-t, --test}]                test against dummy players only\n" +
            "  [{-o, --opponent} agentName]* play named server-side agent\n" +
            "  [{-g, --games} numGames]      numGames to play (optional with -c, default "+NUM_GAMES+")\n" +
            "  [{-s, --setup}] pathname      to pass setup file to server (optional with -c)\n" +
            "  [{-p, --port}] number         port to connect to - default "+ServerConnection.DEFAULT_PORT+"\n" +
            "  [{-l, --log}]                 log game to stdout\n"+
            "default filename is "+DEFAULT_SOURCE);
         System.exit(1);
      }

   } // main
 

   // Flag to prevent re-initializing Prolog in the init method for
   // each game played
   private boolean p_prologInitialized = false;

   // The default name of the Prolog source file to consult
   private final static String DEFAULT_SOURCE = "PrologClient.pl";
   private String p_prologSourceFile; 

   // For readability in Prolog query calls, we define this boolean value
   private final static boolean RETURN_AN_ANSWER = true;

   // Prolog will not accept spaces in the names of symbols and all
   // symbols must begin with a lower-case letter. So we maintain a
   // mapping here from game symbols (e.g., "Mr. Green") to prolog
   // symbols which can be any arbitrary string of characters and vice
   // versa.
   private HashMap<String, String> p_strToPrologSymbol = 
      new HashMap<String, String> ();
   private HashMap<String, String> p_prologSymbolToStr = 
      new HashMap<String, String> ();

} // PrologClient
