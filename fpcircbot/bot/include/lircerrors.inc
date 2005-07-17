{ These are the IRC error numbers according to RFC standard }

const
     ERR_NOSUCHNICK = 401;
{              "<nickname> :No such nick/channel"

      - Used to indicate the nickname parameter supplied to a
        command is currently unused.}

     ERR_NOSUCHSERVER = 402;
{              "<server name> :No such server"

      - Used to indicate the server name given currently
        doesn't exist.}

     ERR_NOSUCHCHANNEL = 403;
{              "<channel name> :No such channel"

      - Used to indicate the given channel name is invalid.}

     ERR_CANNOTSENDTOCHAN = 404;
{              "<channel name> :Cannot send to channel"

      - Sent to a user who is either (a) not on a channel
        which is mode +n or (b) not a chanop (or mode +v) on
        a channel which has mode +m set and is trying to send
        a PRIVMSG message to that channel.}

     ERR_TOOMANYCHANNELS = 405;
{              "<channel name> :You have joined too many \
               channels"
      - Sent to a user when they have joined the maximum
        number of allowed channels and they try to join
        another channel.}

     ERR_WASNOSUCHNICK = 406;
{              "<nickname> :There was no such nickname"

      - Returned by WHOWAS to indicate there is no history
        information for that nickname.}

     ERR_TOOMANYTARGETS = 407;
{              "<target> :Duplicate recipients. No message \

               delivered"

      - Returned to a client which is attempting to send a
        PRIVMSG/NOTICE using the user@host destination format
        and for a user@host which has several occurrences.}

     ERR_NOORIGIN = 409;
{              ":No origin specified"

      - PING or PONG message missing the originator parameter
        which is required since these commands must work
        without valid prefixes.}

     ERR_NORECIPIENT = 411;
//              ":No recipient given (<command>)"
     ERR_NOTEXTTOSEND = 412;
//              ":No text to send"
     ERR_NOTOPLEVEL = 413;
//              "<mask> :No toplevel domain specified"
     ERR_WILDTOPLEVEL = 414;
{              "<mask> :Wildcard in toplevel domain"

      - 412 - 414 are returned by PRIVMSG to indicate that
        the message wasn't delivered for some reason.
        ERR_NOTOPLEVEL and ERR_WILDTOPLEVEL are errors that
        are returned when an invalid use of
        "PRIVMSG $<server>" or "PRIVMSG #<host>" is attempted.}

     ERR_UNKNOWNCOMMAND = 421;
{              "<command> :Unknown command"

      - Returned to a registered client to indicate that the
        command sent is unknown by the server.}

     ERR_NOMOTD = 422;
{              ":MOTD File is missing"

      - Server's MOTD file could not be opened by the server.}

     ERR_NOADMININFO = 423;
{              "<server> :No administrative info available"

      - Returned by a server in response to an ADMIN message
        when there is an error in finding the appropriate
        information.}

     ERR_FILEERROR = 424;
{      ":File error doing <file op> on <file>"

      - Generic error message used to report a failed file
        operation during the processing of a message.}

     ERR_NONICKNAMEGIVEN = 431;
{              ":No nickname given"

      - Returned when a nickname parameter expected for a
        command and isn't found.}

     ERR_ERRONEUSNICKNAME = 432;
{              "<nick> :Erroneus nickname"

      - Returned after receiving a NICK message which contains
        characters which do not fall in the defined set.  See
        section x.x.x for details on valid nicknames.}

     ERR_NICKNAMEINUSE = 433;
{              "<nick> :Nickname is already in use"

      - Returned when a NICK message is processed that results
        in an attempt to change to a currently existing
        nickname.}

     ERR_NICKCOLLISION = 436;
{              "<nick> :Nickname collision KILL"

      - Returned by a server to a client when it detects a
        nickname collision (registered of a NICK that
        already exists by another server).}

     ERR_USERNOTINCHANNEL = 441;
{              "<nick> <channel> :They aren't on that channel"

      - Returned by the server to indicate that the target
        user of the command is not on the given channel.}

     ERR_NOTONCHANNEL = 442;
{              "<channel> :You're not on that channel"

      - Returned by the server whenever a client tries to
        perform a channel effecting command for which the
        client isn't a member.}

     ERR_USERONCHANNEL = 443;
{              "<user> <channel> :is already on channel"

      - Returned when a client tries to invite a user to a
        channel they are already on.}

     ERR_NOLOGIN = 444;
{              "<user> :User not logged in"

      - Returned by the summon after a SUMMON command for a
        user was unable to be performed since they were not
        logged in.}

     ERR_SUMMONDISABLED = 445;
{              ":SUMMON has been disabled"

      - Returned as a response to the SUMMON command.  Must be
        returned by any server which does not implement it.}

     ERR_USERSDISABLED = 446;
{              ":USERS has been disabled"

      - Returned as a response to the USERS command.  Must be
        returned by any server which does not implement it.}

     ERR_NOTREGISTERED = 451;
{              ":You have not registered"

      - Returned by the server to indicate that the client
        must be registered before the server will allow it
        to be parsed in detail.}

     ERR_NEEDMOREPARAMS = 461;
{              "<command> :Not enough parameters"

      - Returned by the server by numerous commands to
        indicate to the client that it didn't supply enough
        parameters.}

     ERR_ALREADYREGISTRED = 462;
{              ":You may not reregister"

      - Returned by the server to any link which tries to
        change part of the registered details (such as
        password or user details from second USER message).}

     ERR_NOPERMFORHOST = 463;
{              ":Your host isn't among the privileged"

      - Returned to a client which attempts to register with
        a server which does not been setup to allow
        connections from the host the attempted connection
        is tried.}

     ERR_PASSWDMISMATCH = 464;
{              ":Password incorrect"

      - Returned to indicate a failed attempt at registering
        a connection for which a password was required and
        was either not given or incorrect.}

     ERR_YOUREBANNEDCREEP = 465;
{              ":You are banned from this server"

      - Returned after an attempt to connect and register
        yourself with a server which has been setup to
        explicitly deny connections to you.}

     ERR_KEYSET = 467;
//              "<channel> :Channel key already set"
     ERR_CHANNELISFULL = 471;
//              "<channel> :Cannot join channel (+l)"
     ERR_UNKNOWNMODE = 472;
//              "<char> :is unknown mode char to me"
     ERR_INVITEONLYCHAN = 473;
//              "<channel> :Cannot join channel (+i)"
     ERR_BANNEDFROMCHAN = 474;
//              "<channel> :Cannot join channel (+b)"
     ERR_BADCHANNELKEY = 475;
//              "<channel> :Cannot join channel (+k)"
     ERR_NOPRIVILEGES = 481;
{              ":Permission Denied- You're not an IRC operator"

      - Any command requiring operator privileges to operate
        must return this error to indicate the attempt was
        unsuccessful.}

     ERR_CHANOPRIVSNEEDED = 482;
{              "<channel> :You're not channel operator"

      - Any command requiring 'chanop' privileges (such as
        MODE messages) must return this error if the client
        making the attempt is not a chanop on the specified
        channel. }

     ERR_CANTKILLSERVER = 483;
{              ":You cant kill a server!"

      - Any attempts to use the KILL command on a server
        are to be refused and this error returned directly
        to the client.}

     ERR_NOOPERHOST = 491;
{              ":No O-lines for your host"

      - If a client sends an OPER message and the server has
        not been configured to allow connections from the
        client's host as an operator, this error must be
        returned. }

     ERR_UMODEUNKNOWNFLAG = 501;
{              ":Unknown MODE flag"

      - Returned by the server to indicate that a MODE
        message was sent with a nickname parameter and that
        the a mode flag sent was not recognized. }

     ERR_USERSDONTMATCH = 502;
{              ":Cant change mode for other users"

      - Error sent to any user trying to view or change the
        user mode for a user other than themselves. }
