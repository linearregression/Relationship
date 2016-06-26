# $1 can be local, localdev, dev stage prod
# $2 shall be the project dir
case $1 in
	local ) NEO_SERVER="http://127.0.0.1:7474/db/data/"
	;;
	localdev ) NEO_SERVER="http://neo4j-dev.sqor.com:7474/db/data/"
	;;
	dev ) NEO_SERVER="http://neo4j-dev.sqor.com:24780/db/data/"
		NEO_USER="dev"
		NEO_PASS="XXXXXXXXXX"
	;;
	stage ) NEO_SERVER="https://neo4j-stage.sqor.com:24780/db/data/"
		NEO_USER="stage"
		NEO_PASS="XXXXXXXXXX"
	;;
	prod ) NEO_SERVER="http://neo4j-prod.sqor.com:XXX/db/data/"
		NEO_USER="tbd"
		NEO_PASS="tbd"
	;;
esac

sed 's/<$!NEO_SERVER!$>/'"$NEO_SERVER"'/' $2/config/template.sys.config > $2/config/sys.config
sed 's/<$!NEO_USER!$>/'"$NEO_USER"'/' $2/config/template.sys.config > $2/config/sys.config
sed 's/<$!NEO_PASS!$>/'"$NEOPASS"'/' $2/config/template.sys.config > $2/config/sys.config
