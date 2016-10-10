%%Programa - Baixar dados do physionet e exportar para arquivos .csv

%Limpando variáveis, linhas de comando e gráficos
clear all; close all; clc;

%Baixando os dados para o "projeto 1"
%[t100, signal100, Fs100]=rdsamp('mitdb/100',[],21601);
%[t101, signal101, Fs101]=rdsamp('mitdb/101',[],21601);
[t102, signal102, Fs102]=rdsamp('mitdb/102',[],21601);
[t103, signal103, Fs103]=rdsamp('mitdb/103',[],21601);
[t104, signal104, Fs104]=rdsamp('mitdb/104',[],21601);
[t105, signal105, Fs105]=rdsamp('mitdb/105',[],21601);
[t106, signal106, Fs106]=rdsamp('mitdb/106',[],21601);
[t107, signal107, Fs107]=rdsamp('mitdb/107',[],21601);
%[t108, signal108, Fs108]=rdsamp('mitdb/108',[],21601);
%[t109, signal109, Fs109]=rdsamp('mitdb/109',[],21601);

%Salvando os dados em uma unica matrix 'signal'
signal=[%t100, signal100(:,1), zeros(21601,1)+100;
        %t101, signal101(:,1), zeros(21601,1)+101;
        t102, signal102(:,1), zeros(21601,1)+102;
        t103, signal103(:,1), zeros(21601,1)+103;
        t104, signal104(:,1), zeros(21601,1)+104;
        t105, signal105(:,1), zeros(21601,1)+105;
        t106, signal106(:,1), zeros(21601,1)+106;
        t107, signal107(:,1), zeros(21601,1)+107];
        %t108, signal108(:,1), zeros(21601,1)+108;
        %t109, signal109(:,1), zeros(21601,1)+109];
    
%Salvando a matrix 'signal' em arquivo .csv
fid = fopen('C:\Users\JoséRoberto\Documents\Documentos\MatLab\mitdb_ecgSignals.csv','w');
fprintf(fid,'%s,%s,%s\n','t','signal_mag','signal_case');
fprintf(fid,'%f,%f,%f\n',signal.');
fclose(fid)

%Salvando a frequencia de amostragem dos sinais
fid = fopen('C:\Users\JoséRoberto\Documents\Documentos\MatLab\fs.csv','w');
fprintf(fid,'%s\n','Fs');
fprintf(fid,'%f\n',Fs102(1).');%,Fs101(1).',Fs102(1).',Fs103(1).',Fs104(1).',Fs105(1).',Fs106(1).',Fs107(1).',Fs108(1).',Fs109(1).');
fclose(fid)


