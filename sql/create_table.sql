use CryptoDB
select * from cryptocurrency where CryptoName = 'polkadot' order by Date desc;

-- drop table cryptocurrency
create table cryptocurrency
(
	Date date not null,
	"Open" float,
	High float,
	Low float,
	"Close" float,
	Volume bigint,
	MarketCap bigint,
	CryptoName varchar(50) not null,
	PRIMARY KEY (Date, CryptoName)
);

select CryptoName, count(*) cnt from cryptocurrency
group by CryptoName
