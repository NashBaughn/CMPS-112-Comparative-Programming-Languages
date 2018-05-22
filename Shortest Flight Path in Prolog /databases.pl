/* 
 * nbaughn@ucsc.edu
 * Nash Baughn
 * CS 112 Spring Quarter 2017
*/

/*
 * Throw error if the destination is the same as departure
*/
fly( Depart, Depart ) :-
    write( 'Error: departure airport == destination airport' ),
    nl, !, 
    fail.

/*
 * Program successefully runs.
*/

fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),

    find_best_path( Depart, Arrive, [Depart], List, _ ),
    !, nl,
    print_path( List ),
    true.

/*
 * Throw error if the flight does not takeoff and land in the same day.
*/

fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    write( 'Error: The complete trip must 
        depart and arrive in the same day.' ),
    nl, !, 
    fail.

/*
 * Throw error if an airport doesnt exist.
*/

fly( _, _) :-
    write( 'Error: airport not found in database' ), 
    nl, !, 
    fail.

/*
 * Time = Distance / Velocity
*/

flight_time(Distance, Time) :-
    Time is Distance / 500.

/*
 * Standard time (Hours:Minutes:Seconds) to Decimal time.
*/

standardtime_to_decimal( time( Hours, Minutes), DecimalTime) :-
    DecimalTime is Hours + Minutes / 60.

/*
 * Prolog's not.
*/

not( X ) :- X, !, fail.
not( _ ).

/*
 * Finds the shortest distance path from 
 * departure airport to destination.
 * The Twightlight Zone Airport requires 
 * a flight to take off and land within the 
 * same day for fear that Rod Serling might appear. 
 * Additionally, the layover time
 * between connection flights must be greater than 30 minutes.
*/

find_best_path( Dest, Dest, _, [Dest], _ ).

/*
 * The current flight can make it to the destination.
*/

find_best_path( Curr, Dest, Visited, 
    [[Curr, DepartTime, ArrivalTime] | List], Depart_ST ) :-
    
    flight( Curr, Dest, Depart_ST ),
    not( member( Dest, Visited ) ),
    
    standardtime_to_decimal( Depart_ST, DepartTime ),
    distance( Curr, Dest, Distance ),
    flight_time( Distance, FlightTime ),
    
    ArrivalTime is DepartTime + FlightTime,
    ArrivalTime < 24.0,
    
    find_best_path( Dest, Dest, [Dest | Visited], List, _).

/*
 * The current flight cannot make it to the destination, 
 * must find a connecting flight.
*/

find_best_path( Curr, Dest, Visited, 
    [[Curr, DepartTime, ArrivalTime] | List],Depart_ST ) :-
    
    flight( Curr, Next, Depart_ST ),
    not( member( Next, Visited ) ),

    standardtime_to_decimal( Depart_ST, DepartTime ),
    distance( Curr, Next, Distance ),
    flight_time( Distance, FlightTime ),
    
    ArrivalTime is DepartTime + FlightTime,
    ArrivalTime < 24.0,

    flight( Next, _, NextDepart_ST ),
    standardtime_to_decimal( NextDepart_ST, NextDepartTime ),
    TransferTime is NextDepartTime - ArrivalTime - 0.5,
    TransferTime >= 0,

    find_best_path( Next, Dest, [Next | Visited], List, NextDepart_ST ).


/*
 * Degrees/Minutes/Seconds to Radians for Haversine formula  
*/

dms_to_rads( degmin( Degrees, Minutes ), Radians ) :-
    Degs is Degrees + Minutes / 60,
    Radians is Degs * pi / 180.

/*
 *  Haversine formula for giving great-circle 
 *  distances between two points on a sphere 
 *  from their longitudes and latitudes.
 *  Interpreted JS code found here: 
 *  http://www.movable-type.co.uk/scripts/latlong.html
*/

haversine( Lat1, Long1, Lat2, Long2, Distance ) :-
    dms_to_rads( Lat1, Lat1_rads ),
    dms_to_rads( Lat2, Lat2_rads ),
    dms_to_rads( Long1, Long1_rads ),
    dms_to_rads( Long2, Long2_rads ),

    DiffLong is Long2_rads - Long1_rads,
    DiffLat is Lat2_rads - Lat1_rads,
    A is sin(DiffLat/2) ** 2 + cos(Lat1_rads) 
        * cos(Lat2_rads) * sin(DiffLong/2) ** 2,
    Dist is 2 * atan2( sqrt(A), sqrt(1 - A)),
    Distance is Dist * 3961.  


/*
 * Finds distance between two Airports using Haversine.
*/

distance( Airport1, Airport2, Distance ) :-
    airport( Airport1, _, Lat1, Long1 ),
    airport( Airport2, _, Lat2, Long2 ),
    haversine( Lat1, Long1, Lat2, Long2, Distance).

/*
 * Print rules/functions
 */
print_digits( Digits ) :-
    Digits < 10, print(0), print(Digits).

print_digits( Digits ) :-
    Digits >= 10, print(Digits).

print_time( In_Hours ) :-
    In_Minutes is floor( In_Hours * 60 ),
    Hours is In_Minutes // 60,
    Minutes is In_Minutes mod 60,
    print_digits( Hours ),
    print( ':' ),
    print_digits( Minutes ).


print_path( [] ) :-
    nl.

/*
 * Prints last flight information before destination. 
 */

print_path( [[Depart, DDTime, DATime], Arrival | []] ) :-
    airport( Depart, Depart_airport, _, _),
    airport( Arrival, Dest_airport, _, _),
    write( '     ' ), write( 'depart   ' ),
    write( Depart ), write( '  ' ),
    write( Depart_airport ),
    print_time( DDTime ), 
    nl,
    write( '     ' ), write( 'arrive    ' ),
    write( Arrival ), write( '  ' ),
    write( Dest_airport ),
    print_time( DATime ), 
    nl,
    !, true.
/*
 * Prints flight information before final flight to destination.
*/
print_path( [[Depart, DDTime, DATime], 
    [Arrival, ADTime, AATime] | Tail] ) :-
    
    airport( Depart, Depart_airport, _, _),
    airport( Arrival, Dest_airport, _, _),
    write( '     ' ), write( 'depart   ' ),
    write( Depart ), write( '  ' ),
    write( Depart_airport ),
    print_time( DDTime ), 
    nl,
    write( '     ' ), write( 'arrive    ' ),
    write( Arrival ), write( '  ' ),
    write( Dest_airport ),
    print_time( DATime ), 
    nl,
    !, print_path( [[Arrival, ADTime, AATime] | Tail] ).
