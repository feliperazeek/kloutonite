# --- !Ups

CREATE TABLE `User` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `twitterHandle` varchar(256) NOT NULL,
  `token` varchar(256) NOT NULL,
  `secret` varchar(256) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_twitterHandle` (`twitterHandle`)
);


# --- !Downs
 
DROP TABLE User;