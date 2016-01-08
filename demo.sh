./rebar compile

for i in `seq 1 10`; do
    erl -pa ebin -s player -s init stop -noinput -noshell | grep -B 4 E
done

