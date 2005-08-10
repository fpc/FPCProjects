case $1 in

create)
	touch markov.txt
	touch words.txt
	;;

delete)
	rm -f markov.txt
	rm -f words.txt
	;;

reset)
        rm -f markov.txt
        rm -f words.txt
        touch markov.txt
        touch words.txt
        ;;

*)
	echo "Usage: makechat {create|delete|reset}"
	exit 1
esac

exit 0
