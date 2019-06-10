NAME=grenier
DUNE=dune

all:
	$(DUNE) build -p $(NAME)

clean:
	$(DUNE) clean -p $(NAME)
test:
	$(DUNE) runtest -p $(NAME)
