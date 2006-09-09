
BEGIN TRANSACTION;

CREATE USER $botdbname PASSWORD '$botdbpass';
CREATE USER $cgidbname PASSWORD '$cgidbpass';

/******************************************************************************/
/****                              Sequences                               ****/
/******************************************************************************/

CREATE SEQUENCE GEN_LOGLINEID;

CREATE SEQUENCE GEN_DEFINITIONID;

CREATE SEQUENCE GEN_CHANNELID;

CREATE SEQUENCE GEN_PASTEID;

/******************************************************************************/
/****                                Tables                                ****/
/******************************************************************************/

CREATE TABLE TBL_LOGLINES (
    LOGLINEID  INTEGER NOT NULL PRIMARY KEY DEFAULT nextval('GEN_LOGLINEID'),
    SENDER     VARCHAR(50),
    RECIEVER   VARCHAR(50),
    MSG        VARCHAR(4096),
    LOGTIME    TIMESTAMP DEFAULT now()
);

CREATE TABLE TBL_DEFINITIONS (
    DEFINITIONID  INTEGER NOT NULL PRIMARY KEY DEFAULT nextval('GEN_DEFINITIONID'),
    DEFINITION    VARCHAR(20) NOT NULL UNIQUE,
    DESCRIPTION   VARCHAR(255)
);

CREATE TABLE TBL_CHANNELS (
    CHANNELID     INTEGER NOT NULL PRIMARY KEY DEFAULT nextval('GEN_CHANNELID'),
    CHANNELNAME   VARCHAR(25) UNIQUE
);

CREATE TABLE TBL_PASTES (
    PASTEID	INTEGER NOT NULL PRIMARY KEY DEFAULT nextval('GEN_PASTEID'),
    TITLE	VARCHAR(25),
    SENDER	VARCHAR(50),
    PASTETIME   TIMESTAMP DEFAULT now(),
    PASTETEXT   TEXT,
    HIGHLIGHT	INTEGER
);

CREATE INDEX tbl_loglines_logtime ON tbl_loglines(logtime);
CREATE INDEX tbl_pastes_pasteid ON tbl_pastes(pasteid);

/******************************************************************************/
/****                              Privileges                              ****/
/******************************************************************************/

/* Privileges of users */

GRANT SELECT ON TBL_LOGLINES TO $cgidbname;
GRANT INSERT ON TBL_LOGLINES TO $botdbname;
GRANT SELECT ON TBL_LOGLINES TO $botdbname;
GRANT SELECT ON GEN_LOGLINEID TO $botdbname;
GRANT UPDATE ON GEN_LOGLINEID TO $botdbname;

GRANT INSERT ON TBL_DEFINITIONS TO $botdbname;
GRANT SELECT ON TBL_DEFINITIONS TO $botdbname;
GRANT UPDATE ON TBL_DEFINITIONS TO $botdbname;
GRANT SELECT ON GEN_DEFINITIONID TO $botdbname;
GRANT UPDATE ON GEN_DEFINITIONID TO $botdbname;

GRANT INSERT ON TBL_CHANNELS TO $botdbname;
GRANT SELECT ON TBL_CHANNELS TO $botdbname;
GRANT UPDATE ON TBL_CHANNELS TO $botdbname;
GRANT SELECT ON TBL_CHANNELS TO $cgidbname;
GRANT SELECT ON GEN_CHANNELID TO $botdbname;
GRANT UPDATE ON GEN_CHANNELID TO $botdbname;

GRANT INSERT ON TBL_PASTES TO $cgidbname;
GRANT SELECT ON TBL_PASTES TO $cgidbname;
GRANT UPDATE ON TBL_PASTES TO $cgidbname;
GRANT DELETE ON TBL_PASTES TO $cgidbname;
GRANT SELECT ON GEN_PASTEID TO $cgidbname;
GRANT UPDATE ON GEN_PASTEID TO $cgidbname;

COMMIT TRANSACTION;
