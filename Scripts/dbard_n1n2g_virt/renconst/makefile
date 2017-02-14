LIBS += $(LIB)
VPATH := $(VPATH):$(DIR)
OBJS_RC :=

OBJS := \
  $(DIR)RenConst.o

OBJS_RC += $(OBJS)

$(LIB)($(OBJS)): F-renconst.d $(DIR)RenConst.h


$(LIB): $(LIB)($(OBJS_RC))

