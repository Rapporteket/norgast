CREATE DATABASE IF NOT EXISTS db_data
  DEFAULT
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

CREATE DATABASE IF NOT EXISTS db_log
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

USE db_log;

CREATE TABLE IF NOT EXISTS `appLog` (
  `id` int(9) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(127) COLLATE utf8_danish_ci DEFAULT NULL,
  `name` varchar(255) COLLATE utf8_danish_ci DEFAULT NULL,
  `group` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `role` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `resh_id` varchar(31) COLLATE utf8_danish_ci DEFAULT NULL,
  `message` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;


CREATE TABLE IF NOT EXISTS `reportLog` (
  `id` int(9) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(127) COLLATE utf8_danish_ci DEFAULT NULL,
  `name` varchar(255) COLLATE utf8_danish_ci DEFAULT NULL,
  `group` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `role` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `resh_id` varchar(31) COLLATE utf8_danish_ci DEFAULT NULL,
  `environment` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `call` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  `message` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;


CREATE DATABASE IF NOT EXISTS db_autoreport
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

use db_autoreport;

CREATE TABLE IF NOT EXISTS `autoreport` (
  `j` JSON
  CHECK (JSON_VALID(j))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

INSERT INTO `autoreport` VALUES ('{\"type\":\"list\",\"attributes\":{\"names\":{\"type\":\"character\",\"attributes\":{},\"value\":[\"testAutoReportFirst\"]}},\"value\":[{\"type\":\"list\",\"attributes\":{\"names\":{\"type\":\"character\",\"attributes\":{},\"value\":[\"synopsis\",\"package\",\"type\",\"fun\",\"params\",\"owner\",\"email\",\"organization\",\"startDate\",\"terminateDate\",\"interval\"]}},\"value\":[{\"type\":\"character\",\"attributes\":{},\"value\":[\"Test, the first one\"]},{\"type\":\"character\",\"attributes\":{},\"value\":[\"rapbase\"]},{\"type\":\"character\",\"attributes\":{},\"value\":[\"dispatchment\"]},{\"type\":\"character\",\"attributes\":{},\"value\":[\".testAutoReport\"]},{\"type\":\"list\",\"attributes\":{},\"value\":[{\"type\":\"list\",\"attributes\":{\"names\":{\"type\":\"character\",\"attributes\":{},\"value\":[\"aNum\"]}},\"value\":[{\"type\":\"integer\",\"attributes\":{},\"value\":[1]}]},{\"type\":\"list\",\"attributes\":{\"names\":{\"type\":\"character\",\"attributes\":{},\"value\":[\"aChar\"]}},\"value\":[{\"type\":\"character\",\"attributes\":{},\"value\":[\"a\"]}]},{\"type\":\"list\",\"attributes\":{\"names\":{\"type\":\"character\",\"attributes\":{},\"value\":[\"anExp\"]}},\"value\":[{\"type\":\"character\",\"attributes\":{},\"value\":[\"Sys.Date()\"]}]}]},{\"type\":\"character\",\"attributes\":{},\"value\":[\"ttester\"]},{\"type\":\"character\",\"attributes\":{},\"value\":[\"Some One <someone@nowhere.com>\",\"Jesus <jesus@sky.com>\"]},{\"type\":\"character\",\"attributes\":{},\"value\":[\"999999\"]},{\"type\":\"character\",\"attributes\":{},\"value\":[\"1900-01-01\"]},{\"type\":\"character\",\"attributes\":{},\"value\":[\"9999-12-31\"]},{\"type\":\"list\",\"attributes\":{\"names\":{\"type\":\"character\",\"attributes\":{},\"value\":[\"intervalName\",\"runDayOfYear\"]}},\"value\":[{\"type\":\"NULL\"},{\"type\":\"integer\",\"attributes\":{},\"value\":[30,60]}]}]}]}');
