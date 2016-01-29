#*****************************************************************************
#                Makefile Build System for Fawkes: MQTT bits
#                            -------------------
#   Created on Thu Dec 03 00:58:00 2015 (Magdeburg)
#   Copyright (C) 2015 by Kai Seidensticker
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

ifndef __buildsys_config_mk_
$(error config.mk must be included before mqtt.mk)
endif

ifndef __buildsys_libmqtt_mk_
__buildsys_libmqtt_mk_ := 1

LIBMQTT_MIN_VERSION=1.4.5

ifneq ($(PKGCONFIG),)
  HAVE_LIBMOSQUITTOPP = $(if $(shell $(PKGCONFIG) --atleast-version=$(LIBMQTT_MIN_VERSION) 'libmosquittopp'; echo $${?/1/}),1,0)
endif

ifeq ($(HAVE_LIBMOSQUITTOPP),1)
  CFLAGS_LIBMOSQUITTOPP  = -DHAVE_LIBMOSQUITTOPP $(shell $(PKGCONFIG) --cflags 'libmosquittopp')
  LDFLAGS_LIBMOSQUITTOPP = $(shell $(PKGCONFIG) --libs 'libmosquitto')
endif

endif # __buildsys_libmosquittopp_mk_

