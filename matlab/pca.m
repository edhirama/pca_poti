% @head: MAI5002 - Fundamentos de Matemática Aplicada
% @auth: André Perez
% @mail: andre.marcos.perez@usp.br
% -------------------------------------------------------------------------

%% PCA
% -------------------------------------------------------------------------

clear;
clc;

%% Read data

FILENAME = '..\data\facebook.csv';
facebook = import(FILENAME)';

nrow = size(facebook,1);
ncol = size(facebook,2);

%% Center data

for i = 1:ncol
   
    facebook(:,i) = facebook(:,i) / mean(facebook(:,i));
    
end

%% Normalize data

%% Clear

clear FILENAME
clear i
clear nrow
clear ncol
