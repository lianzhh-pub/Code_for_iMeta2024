for i in `cat  lst2`;
do
    for j in `cat lst`;
    do
        out=$(java -jar OAT_cmd.jar -blastplus_dir /usr/local/bin/ -num_threads 10 -fasta1 input/$i -fasta2 input/$j|grep OrthoANI |sed -e 's/OrthoANI : //' -e 's/ (%)//');
        echo -e $i"\t"$j"\t"$out >> ANI.out3;
        out=$(java -jar OAT_cmd.jar -blastplus_dir /usr/local/bin/ -num_threads 10 -fasta2 input/$i -fasta1 input/$j|grep OrthoANI |sed -e 's/OrthoANI : //' -e 's/ (%)//');
        echo -e $i"\t"$j"\t"$out >> ANI.out3;
    done
done
