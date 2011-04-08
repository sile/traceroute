(in-package :traceroute)

(declaim #.*muffle-compiler-note*)

(eval-when (:load-toplevel)
  (define-alien-routine setsockopt int (sockfd int) (level int) (optname int) 
                                       (optval (* t)) (optlen socklen_t))
  (define-alien-routine socket int (domain int) (type int) (protocol int))
  (define-alien-routine sendto int (sockfd int) (buf (* t))
                                   (len size_t) (flags int)
                                   (dest_addr (* sockaddr-in)) (addrlen socklen_t))
  (define-alien-routine recv int (socketfd int) (buf (* t)) (len size_t) (flags int)))

(defun set-ip-ttl (socket ttl)
  (declare #.*muffle-compiler-note*)
  (with-alien ((*ttl (array int 1)))
    (setf (deref *ttl 0) ttl)
    (or (= 0 (setsockopt socket +IPPROTO_IP+ +IP_TTL+
                        (cast *ttl (* t)) (alien-size int :bytes)))
        (progn (sb-unix:unix-close socket) nil))))

(defun make-socket-fd (domain type protocol &key ttl)
  (let ((fd (socket domain type protocol)))
    (if (= fd -1)
        (values nil (get-errno) (sb-int:strerror (get-errno)))
      (if (set-ip-ttl fd ttl)
          fd
        nil))))

(defun init-sockaddr-in (sa ip)
  (setf (sockaddr-in.family sa) +AF_INET+
        (sockaddr-in.port sa) 0
        (sockaddr-in.addr sa) (ip-to-addr ip))
  sa)

(defun init-icmp-header (icmp &key type sequence)
  (memset icmp 0 icmp-header.size)
  (setf (icmp-header.type icmp) type
        (icmp-header.seq-num icmp) sequence
        (icmp-header.checksum icmp) (checksum icmp icmp-header.size))
  icmp)

(defun icmp-sendto (socket icmp sa flags)
  (/= -1 (sendto socket
                 (cast icmp (* t)) icmp-header.size flags
                 (cast sa (* sockaddr-in)) sockaddr-in.size)))

(defun icmp-recv (socket packet flags)
  (case (recv socket (cast packet (* t)) packet.size flags)
    (-1 :error)
    (0  :shutdown)
    (t  :ok)))

(defun get-echo-message (icmp)
  (case (icmp-header.type icmp)
    (#.+ICMP_ECHO+ "LOOPBACK")
    (#.+ICMP_ECHO_REPLY+ "REPLY")
    (#.+ICMP_UNREACH+
     (format nil "UNREACHED - ~a"
             (documentation (nth (icmp-header.code icmp) *icmp-unreach-types*)
                            'variable)))
    (#.+ICMP_TIMEXCEED+
     (format nil "TIME-EXCEEDED(code:~d)" (icmp-header.code icmp)))
    (otherwise
     (format nil "UNKNOWN TYPE: ~d" (icmp-header.type icmp)))))

(defmacro throw-error (tag &key (alien-error t))
  `(throw :error
          (list ,tag 
                (when ,alien-error (format nil "~A[~D]" (sb-int:strerror (get-errno)) (get-errno))))))

(defun echo-and-reply (sock icmp sa flags packet seq-num)
  (let ((took 
          (timing
            (unless (icmp-sendto sock icmp sa flags)
              (throw-error :sendto-error))
            (case (icmp-recv sock packet 0)
              (:error (throw-error :recv-error))
              (:shutdown (throw-error :recv-peer-closed :alien-error nil)))))
        (reply-ip (slot packet 'ip))
        (reply-icmp (slot packet 'icmp)))
    (log-msg "From ~/traceroute::ip-fmt/: icmp_seq=~d ttl=~d time=~d ms: ~a"
             (ip-header.src-addr reply-ip) seq-num (ip-header.ttl reply-ip) (round took)
             (get-echo-message reply-icmp))))

(defun trace-loop (sa ttl max-ttl interval flags)
  (if (> ttl max-ttl)
      `(:ttl-limit-exceeded ,ttl)
    (with-alien ((icmp icmp-header)
                 (packet packet))
      (init-icmp-header icmp :type +ICMP_ECHO+ :sequence ttl)
      (let ((sock (make-socket-fd +AF_INET+ +SOCK_RAW+ +IPPROTO_ICMP+ :ttl ttl)))
        (unless sock
          (throw-error :make-socket-error))
        
        (unwind-protect
            (echo-and-reply sock icmp sa flags packet ttl)
          (sb-unix:unix-close sock))
        (if (case (icmp-header.type (packet.icmp packet))
              ((#.+ICMP_ECHO_REPLY+ #.+ICMP_ECHO+) t))
            'done
          (progn (sleep interval)
                 (trace-loop sa (1+ ttl) max-ttl interval flags)))))))

(defun trace (target &key (max-ttl 30) (interval 0.5))
  (log-msg "trace routes to ~a~@[ (~/traceroute::ip-fmt/)~]" target 
           (to-network-order (ip-to-addr (resolve-address target)) 4))
  (with-alien ((sa sockaddr-in))
    (n.if (ip (resolve-address target))
          (catch :error
            (trace-loop (init-sockaddr-in sa ip) 1 max-ttl interval 0))
      `(:unknown-host ,target))))
