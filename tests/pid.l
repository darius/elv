(write-char (cond ((pid? (self)) \a) ('t \b)))
(write-char (cond ((pid? 0) \a) ('t \b)))
