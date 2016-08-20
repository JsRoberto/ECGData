%%Programa - Baixar dados do physionet e exportar para arquivos .csv e .txt

%Limpando variáveis, linhas de comando e gráficos
clear all; close all; clc;

%Baixando os dados para o "projeto 1"
[t100, signal100, Fs100]=rdsamp('mitdb/100',[],21601);
[t101, signal101, Fs101]=rdsamp('mitdb/101',[],21601);
[t102, signal102, Fs102]=rdsamp('mitdb/102',[],21601);
[t103, signal103, Fs103]=rdsamp('mitdb/103',[],21601);
[t104, signal104, Fs104]=rdsamp('mitdb/104',[],21601);
[t105, signal105, Fs105]=rdsamp('mitdb/105',[],21601);
[t106, signal106, Fs106]=rdsamp('mitdb/106',[],21601);
[t107, signal107, Fs107]=rdsamp('mitdb/107',[],21601);
[t108, signal108, Fs108]=rdsamp('mitdb/108',[],21601);
[t109, signal109, Fs109]=rdsamp('mitdb/109',[],21601);

%Salvando os dados em uma unica matrix 'signal'
signal=[signal100(:,1), signal101(:,1), signal102(:,1), signal103(:,1), signal104(:,1), signal105(:,1), signal106(:,1), signal107(:,1), signal108(:,1), signal109(:,1)];
    
%Salvando a matrix 'signal' em arquivo .csv
fid = fopen('C:\Users\JoséRoberto\Documents\Documentos\MatLab\mitdb_ecgSignals.csv','w');
fprintf(fid,'%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n','signal_mag100','signal_mag101','signal_mag102','signal_mag103','signal_mag104','signal_mag105','signal_mag106','signal_mag107','signal_mag108','signal_mag109');
fprintf(fid,'%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n',signal.');
fclose(fid)

%Salvando a frequencia de amostragem dos sinais
fid = fopen('C:\Users\JoséRoberto\Documents\Documentos\MatLab\fs.csv','w');
fprintf(fid,'%s\n','Fs');
fprintf(fid,'%f\n',Fs100(1).',Fs101(1).',Fs102(1).',Fs103(1).',Fs104(1).',Fs105(1).',Fs106(1).',Fs107(1).',Fs108(1).',Fs109(1).');
fclose(fid)


