%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: path_util.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri Dec 26 22:10:56 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Mongoose URI helper.
%----------------------------------------------------------------------------%

:- module mercury_mongoose.path_util.

:- interface.

:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

:- type uri_encoded_path.

    % split_path(Path) = Components:
    % NOTE: Leading slash is stripped
    %
:- func split_path(uri_encoded_path) = list(string).

:- func to_encoded_string(uri_encoded_path) = string.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

:- type uri_encoded_path == string.

split_path(Path) =
    ( if [_ | Split] = split_at_char('/', Path) then Split else [] ).

to_encoded_string(Uri) = Uri.

%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.path_util.
%----------------------------------------------------------------------------%
