-module(local_queue).

-export([should_stop/3, next_direction/3, add_order/5]).

-define(MaxFloor, 4-1). %MaxFloorumber of floors

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% FUCNTIONS FOR fsm_elevator %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

should_stop(Queue, CurrFloor, Dir) ->
  case (lists:keymember({CurrFloor,cab}, 2, Queue)) of
      true ->
        true;
      false ->
        case (lists:keymember({CurrFloor,Dir}, 2, Queue)) of
          true ->
            true;
          false ->
            case Dir of
              hall_up ->
                case lists:keymember({CurrFloor,hall_down}, 2, Queue) of
                  true ->
                    case queue_check_upwards(Queue, CurrFloor, hall_up) of
                      true ->
                        false;
                      false ->
                        true
                    end;
                  false ->
                    false
                end;
              hall_down ->
                case lists:keymember({CurrFloor,hall_up}, 2, Queue) of
                  true ->
                    case queue_check_downwards(Queue, CurrFloor, hall_down) of
                      true ->
                        false;
                      false ->
                        true
                    end;
                  false ->
                    false
               end
            end
      end
    end.

next_direction(Queue, CurrFloor, Dir) ->
  case Dir of
    hall_up ->
      case queue_check_upwards(Queue, CurrFloor, Dir) of
        true -> hall_up;
        false ->
          case queue_check_downwards(Queue, CurrFloor, Dir) of
            true -> hall_down;
            false -> idle
        end
      end;
    hall_down ->
      case queue_check_downwards(Queue, CurrFloor, Dir) of
        true -> hall_down;
        false ->
          case queue_check_upwards(Queue, CurrFloor, Dir) of
            true -> hall_up;
            false -> idle
        end
      end
    end.

add_order(_Pid,Queue, Ref, Floor, Type)->
case lists:keymember({Floor,Type},2, Queue) of
    true->
      Queue,
      {ok,Ref};
    false->
    %  set_order_button_light(Pid, Type, Floor, on),
      [{Ref,{Floor,Type}}|Queue]
end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

queue_check_upwards(Queue, CurrFloor, Dir) when ?MaxFloor - CurrFloor > 0->
  case should_stop(Queue, CurrFloor + 1, Dir) of
     true -> true;
     false ->
      queue_check_upwards(Queue, CurrFloor + 1, Dir)
      %if not, let me check if next floor has cab/up orders until N - CurrFloor + 1 =:= 0
  end;
queue_check_upwards(_Queue, CurrFloor, _Dir) when ?MaxFloor - CurrFloor =:= 0->
  false.

queue_check_downwards(Queue, CurrFloor, Dir) when CurrFloor > 0 ->
  case should_stop(Queue, CurrFloor - 1, Dir) of
     true -> true;
     false ->
        queue_check_downwards(Queue, CurrFloor - 1, Dir)
        %if not, let me check if next floor has cab/up orders until N - CurrFloor + 1 =:= 0
  end;

queue_check_downwards(_Queue, CurrFloor, _Dir) when CurrFloor =:= 0 ->
  false.
