			/*  STP CLIENT   */

/*  Client side of the Seismic Transfer Program
 *  R. Clayton, Caltech, Sept, 2000
 *  Modified 2002/7/03 to include inout line editting based
 *       on a version by Brian Savage.
 */

/*
 * On Solaris compile as:
 *	cc -o stp stp.c -lresolv -lsocket
 * On Linux compile as:
 *	cc -o stp stp.c -lresolv 
 */

void open_waveserver();
void close_waveserver();
void ws_putline();
void create_name();
int getkey();
int ws_getline();
int ws_getdata();
int ws_putdata();
void receive_data();
void do_commands();
void release_data();
int makedirectory();
void usage();
void make_upper_case();
char * my_getline();

	
#include	<stdio.h>
#include	<time.h>
#include	<sys/types.h>	/* basic system data types */
#include	<sys/socket.h>	/* basic socket definitions */
#include	<netinet/in.h>	/* sockaddr_in{} and other Internet defns */
#include	<arpa/inet.h>	/* inet(3) functions */
#include	<errno.h>
#include	<fcntl.h>		/* for nonblocking */
#include	<netdb.h>
#include	<signal.h>
#include	<sys/un.h>		/* for Unix domain sockets */

#define	socklen_t unsigned int		/* <sys/socket.h> */
#define SA struct sockaddr
#define EDITLINE 1

#ifdef EDITLINE
#include "editline.h"
char *readline();
#endif

#define PASSWD	"hastalavista"
#define VERSION	"1.4"
#define MAXLINE	256
#define DATABUFSZ	10240
char databuf[DATABUFSZ];
int verbose	= 0;
int nrowset	= 24;
char masterdir[256] = ".";
FILE *ws_fdr, *ws_fdw;
int ws_sockfd;
int iserv;

struct address
   {
	char	name[16];
	char	addr[64];
	int	port;
   };


struct address address[] = {
   {	"VEOXstp","mase.gps.caltech.edu",		9992 },
   { 	"",	"000.000.000.000", 		0 }
   };

#define WARN	0
#define FATAL	1
FILE *cpyout	= NULL;
char cpyfile[256];

int
main(int ac, char **av)
{
  int iserv;
  char addr[64] = "";
  ac--; av++;
  while(ac > 0)
    {
      if(av[0][0] == '-')
	{
	  switch(av[0][1])
	    {
	    case 'v': 
	      verbose= 1;
	      ac--; av++;
	      break;
	    case 'd': 
	      if (ac > 1) {
		strcpy(masterdir,av[1]);
		ac--; av++;
	      }
	      ac--; av++;
	      break;
	    case 'a': 
	      if (ac > 1) {
		strcpy(addr,av[1]);
		ac--; av++;
	      }
	      ac--; av++;
	      break;
	    case 'p': 
	      if (ac > 1) {
		for(iserv= 0; address[iserv].name[0] != '\0'; iserv++)
		  address[iserv].port= atol(av[1]);
		ac--; av++;
	      }
	      ac--; av++;
	      break;
	    case 'h': 
	      usage();
	      exit(0);
	    default:
	      fprintf(stderr,"stp: bad flag =%c\n",av[0][1]);
	      usage();
	      exit(-1);
	    }
	}
      else break;
    }
  
  /* open connection to the waveserver */
  if(strcmp(addr, "") != 0) open_waveserver(addr);
  else      open_waveserver("*");
  
  /* process the input commands */
  do_commands(stdin);
  
  close_waveserver();
  exit(0);
}

void
do_commands(FILE *input)
{
  char line[MAXLINE+1], com[64];
  FILE *fopen();
  
  while( 1 )
	  {
	    /*
	      if(input == stdin)
	      {
	      fprintf(stdout,"STP> ");
	      fflush(stdout);
	      }
	      if ( fgets(line,MAXLINE,input) == NULL ) break;
	    */
	    if( my_getline(line,MAXLINE,input) == NULL ) break;
	    if(line[0] == '!')  /* shell escape */
	      {
		system(&line[1]);
		continue;
	      }
	    if(sscanf(line,"%s",com) < 1) continue;
	    make_upper_case(com);
	    
	    /* first see if this is a local command */
	    /* EXIT */
	    if((strcmp("EXIT",com) == 0) || (strcmp("QUIT",com) == 0))
	      {
		close_waveserver();
		exit(0);
	      }
	    
	    /* INPUT */
	    if((strcmp("INPUT",com) == 0) || (strcmp("IN",com) == 0))
	      {
		FILE *fd;
		char filename[256];
		if( sscanf(line,"%s %s",com,filename) != 2)
		  {
		    err(WARN,"must specify file name in INPUT command\n");
		    continue;
		  }
		if( (fd= fopen(filename,"r")) == NULL)
		  {
		    err(WARN,"cannot open file =%s\n",filename);
		    continue;
		  }
			do_commands(fd);
			fclose(fd);
			continue;
	      }
	    /* OUTPUT */
	    if((strcmp("OUTPUT",com) == 0) || (strcmp("OUT",com) == 0))
	      {
		if( sscanf(line,"%s %s",com,cpyfile) != 2)
		  {
		    err(WARN,"must specify file name or 'OFF'  in OUPUT command\n");
		    continue;
		  }
		if(strcmp(cpyfile,"OFF")==0 || strcmp(cpyfile,"off")==0)
		  {
		    if(cpyout == NULL)
		      {
			err(WARN,"output not already being copied\n");
			continue;
		      }
		    fclose(cpyout);
		    cpyout= NULL;
		    report("copying is off\n");
		    continue;
		  }
		if( (cpyout= fopen(cpyfile,"w")) == NULL)
		  {
		    err(WARN,"cannot output file =%s\n",cpyfile);
		    continue;
		  }
		continue;
	      }
	    
	    /* VERBOSE */
	    if((strcmp("VERBOSE",com) == 0) || (strcmp("V",com) == 0))
	      {
		if(verbose)
		  {
		    fprintf(stderr,"verbose is off\n");
		    verbose= 0;
		  }
		else
		  {
		    fprintf(stderr,"verbose is on\n");
		    verbose= 1;
		  }
		continue;
	      }
	    /* SET - check if we are setting some local stuff */
	    if((strcmp("SET",com) == 0) || (strcmp("S",com) == 0))
	      {
		char name[64], val[64];
		if( sscanf(line,"%s %s %s",com,name,val) != 3)
		  {
		    fprintf(stderr,"invalid set command\n");
		    continue;
		  }
		if(strcmp("NLINE",name) == 0)
		  {
		    nrowset= atol(val);
		    if(nrowset <= 0) nrowset= 24;
		    continue;
		  }
		/* fall out the bottom, so server-side set's are done */
		   }
	    /* STATUS */
	    if((strcmp("STATUS",com) == 0) || (strcmp("S",com) == 0))
	      {
		fprintf(stderr,"Client Status:\n");
		fprintf(stderr,"\tVerbose = %d\n",verbose);
		fprintf(stderr,"\tNline   = %d\n",nrowset);
		fprintf(stderr,"\tOutput  = %s\n",
			(cpyout == NULL ? "off": cpyfile ));
		fprintf(stderr,"\tServer  = %s\n",address[iserv].name);
		fprintf(stderr,"\tAddress = %s\n",address[iserv].addr);
		fprintf(stderr,"\tPort    = %d\n",address[iserv].port);
			/* report from server */
		ws_putline(line);
		receive_data();
		continue;
	      }
	    /* if we get here, the command is to handed to the server */
	    ws_putline(line);
	    receive_data();
	  }
}

char *
my_getline(char *line, int max, FILE *input)
{
#ifdef EDITLINE
  static char *line_read = (char *)NULL;
  
  if(input != stdin) /* use non-edit input for files*/
    {
      if ( fgets(line,max,input) == NULL )
	return(NULL);
		return(line);
    }
  if(line_read)
    {
      free(line_read);
      line_read= (char *)NULL;
    }
  line_read= readline("STP> ");
  if(line_read) {
    if(*line_read)
      add_history(line_read);
  }
  else
    return(NULL);
  if(strlen(line_read) >= max-1) line_read[max-2]= '\0';
  /* we need a newline on the string */
  sprintf(line,"%s\n",line_read);
#else
  if(input == stdin)
    {
      fprintf(stdout,"STP> ");
      fflush(stdout);
    }
  if ( fgets(line,max,input) == NULL ) return(NULL);
#endif /* EDITLINE */
  return(line);
}

void
receive_data()
{
  char line[MAXLINE+1], com[MAXLINE+1];
  char fname[256], filename[256], dir[256], tempdir[256];
  FILE *fdout, *fopen();
  int ndata, n, len;

  /* directory specifications from the server on last over
     one command */
  strcpy(dir,masterdir);
  fdout= NULL;
  
	while( ws_getline(line,MAXLINE) != EOF)
	  {
	    sscanf(line,"%s",com);
	    
	    if(strcmp("OVER",com) == 0)
	      {
		/* close any open files */
		if(fdout != NULL) fclose(fdout);
		return;
	      }
	    if(strcmp("FILE",com) == 0)
	      {
		if(fdout != NULL) fclose(fdout);
		sscanf(line,"%s %s",com,fname);
		/* creat file */
		sprintf(filename,"%s/%s",dir,fname);
		if( (fdout= fopen(filename,"w")) == NULL)
		  err(WARN,"cannot create %s\n",filename);
		report("create file =%s\n",fname);
		continue;
	      }
	    if(strcmp("DIR",com) == 0)
	      {
		sscanf(line,"%s %s",com,tempdir);
		sprintf(dir,"%s/%s",masterdir,tempdir);
		if(makedirectory(dir) < 0)
		  err(FATAL,"cannot make directory %s\n", dir);
			report("create directory =%s\n",tempdir);
			continue;
	      }
	    if(strcmp("DATA",com) == 0)
	      {
		int ndatahold;
		sscanf(line,"%s %d",com,&ndata);
		ndatahold= ndata;
		while(ndata > 0)
		  {
		    len= (ndata > DATABUFSZ ? DATABUFSZ : ndata);
		    n= fread(databuf,1,len,ws_fdr);
		    if(n <= 0)
		      err(FATAL,"read error from server\n");
		    if(n == 0) continue;
		    ndata -= n;
		    if(fdout == NULL) continue;
		    if(fwrite(databuf,1,n,fdout) != n)
		      err(FATAL,"write error to file %s\n",filename);
		  }
		if( ws_getline(line,MAXLINE) == EOF)
		  err(FATAL,"read read form server\n");
		/* final intergrity check on data transfer */
		sscanf(line,"%s",com);
		if(strcmp("ENDdata",com) != 0)
		  err(FATAL,"read read form server\n");
		report("\tdata nbytes=%d\n",ndatahold);
		continue;
	      }
	    if(strcmp("MESS",com) == 0)
	      {
		int cnt, nrow;
		char buf[24];
		nrow=nrowset;
		cnt= 0;
		/* transfer verbatim messages */
		while( ws_getline(line,MAXLINE) != EOF)
		  {
		    sscanf(line,"%s",com);
		    if(strcmp("ENDmess",com) == 0) break;
		    cnt++;
		    if(cnt%nrow == 0)
		      {
			fprintf(stdout,"%5d -- return -->",cnt);
			fflush(stdout);
			fgets(buf,32,stdin);
			if(buf[0] >= '0' && buf[0] <= '9') nrow= atol(buf);
			if(buf[0] == 'q' || buf[0] == 'Q')
			  {
			    /* suck the remaining out of the input */
			    while( ws_getline(line,MAXLINE) != EOF)
			      {
				sscanf(line,"%s",com);
				if(strcmp("ENDmess",com) == 0) break;
			      }
			    break;
			  }
		      }
		    fprintf(stdout,"%s",line);
		    if(cpyout) fprintf(cpyout,"%s",line);
		  }
		continue;
	      }
	    if(strcmp("ERR",com) == 0)
	      {
		/* transfer 1-line error message */
		fprintf(stderr,"Error Message;\n");
		fprintf(stderr,"%s",&line[3]);
		if(cpyout) fprintf(cpyout,"Error Message;\n");
		if(cpyout) fprintf(cpyout,"%s",&line[3]);
		continue;
	      }
	    
	  }
}


void
open_waveserver(char *server_name)
{
  int two;
  FILE *fdopen();
  struct sockaddr_in servaddr, cliaddr;
  struct hostent *hp;
  struct in_addr **pptr;
  char line[64];
  /*socklen_t len;*/
  uint len;
  
  /* see if caller if fussy about which server */
  /* if not fussy, try each one in case of error */
  if(strcmp(server_name,"*") == 0)
    {
      for(iserv= 0; address[iserv].name[0] != '\0'; iserv++)
	{
	  /* get IP number of stp server */
	  if( (hp= gethostbyname(address[iserv].addr)) == NULL )
	    {
	      err(WARN,"STP: gethostbyname error %s\n",address[iserv].addr);
	      continue;
	    }
	  pptr= (struct in_addr **)hp->h_addr_list;
	  /* set up a socket */
	  if ( (ws_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	    err(WARN,"Wave_Server: socket connect error\n");
	  /* connect socket to Wave Server Port */
	  bzero(&servaddr, sizeof(servaddr));
	  servaddr.sin_family = AF_INET;
	  servaddr.sin_port   = htons(address[iserv].port);
	  memcpy(&servaddr.sin_addr,*pptr,sizeof(struct in_addr));
	  
	  if (connect(ws_sockfd, (SA *) &servaddr, sizeof(servaddr)) < 0) continue;
	  
	  len = sizeof(cliaddr);
	  if (getsockname(ws_sockfd, (SA *) &cliaddr, (int *)(&len)) < 0)
	    err(WARN,"STP: getsockname error");
	  
	  ws_fdr= fdopen(ws_sockfd,"r");
	  ws_fdw= fdopen(ws_sockfd,"w");
	  if( ws_fdr == NULL || ws_fdw == NULL)
	    err(FATAL,"STP: cannot open ws_fdw/ws_fdr\n");
	  
	  /* send authorization */
	  sprintf(line,"%s %s %s %s\n","STP",PASSWD,VERSION, "stpc");
	  ws_putline(line);
	  fflush(ws_fdw);
	  
	ws_getline(line,MAXLINE);
	/* check for a login request */
	while(strcmp(line,"Login:\n") == 0)
	   {
		char login[64], *passwd, *getpass(), buf[64];
		fprintf(stdout,"Login: ");
		fflush(stdout);
		fscanf(stdin,"%s",login);
		if( (passwd= getpass("Password: ")) == NULL )
			fprintf(stderr,"getpass error\n");
		sprintf(buf,"%s %s\n",login,passwd);
		ws_putline(buf);
		ws_getline(line,MAXLINE);
	   }
	if(strcmp(line,"Invalid Login\n") == 0)
		err(FATAL,"STP: Connection rejected by waveserver - %s\n", line);

	  /* get connection acknowledgement */
	  /*ws_getline(line,MAXLINE);*/
	  if(strcmp(line,"CONNECTED\n") != 0) {
	    err(WARN,"STP: Connection rejected by waveserver - %s\n", line);
	    continue;
	  }

	  fprintf(stderr,"STP: Connected to %s\n",address[iserv].addr);
	  
	  /* send a binary sample so the server can establish byte order */
	  two= 2;
	  ws_putdata(&two,4);
	  fflush(ws_fdw);
	  
	  /* receiver possible message from server */
	  receive_data();
	  break;
	}
      if(address[iserv].name[0] == 0)
	err(FATAL,"STP: connect error\n");
    }
  else
    {
      char serv[64] = "", test[64] = "";

      /* find server in table */
      for(iserv= 0; address[iserv].name[0] != '\0'; iserv++) {
	/* compare text addresses */
	if(strcmp(address[iserv].addr,server_name) == 0) break;


	/* compare by IP address */
	hp = gethostbyname(server_name);
	if (hp != NULL) {
	  pptr= (struct in_addr **)hp->h_addr_list;
	  strcpy(serv, inet_ntoa(**pptr));	
	}
	hp = gethostbyname(address[iserv].addr);
	if (hp != NULL) {
	  pptr= (struct in_addr **)hp->h_addr_list;
	  strcpy(test, inet_ntoa(**pptr));	
	} 
	/* both have the same IP */
	if (strcmp(test, serv) == 0) break;
	/* the given value is the IP of this address */
	if (strcmp(test, server_name) == 0) break;

	
	/* compare names */
	if(strcmp(address[iserv].name,server_name) == 0) break;
      }

      if(address[iserv].name[0] == '\0')
	err(FATAL,"Unknown server = %s\n",server_name);
      
      /* get IP number of stp server */
      if( (hp= gethostbyname(address[iserv].addr)) == NULL )
	err(FATAL,"STP: gethostbyname error\n");
      pptr= (struct in_addr **)hp->h_addr_list;



      /* set up a socket */
      if ( (ws_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	err(FATAL,"Wave_Server: socket connect error\n");

      /* connect socket to Wave Server Port */
      bzero(&servaddr, sizeof(servaddr));
      servaddr.sin_family = AF_INET;
      servaddr.sin_port   = htons(address[iserv].port);
      memcpy(&servaddr.sin_addr,*pptr,sizeof(struct in_addr));

      if (connect(ws_sockfd, (SA *) &servaddr, sizeof(servaddr)) < 0)
	err(FATAL,"STP: connect error\n");
      
      len = sizeof(cliaddr);
      if (getsockname(ws_sockfd, (SA *) &cliaddr, (int *)(&len)) < 0)
	err(WARN,"STP: getsockname error");
      
      ws_fdr= fdopen(ws_sockfd,"r");
      ws_fdw= fdopen(ws_sockfd,"w");
	if( ws_fdr == NULL || ws_fdw == NULL)
	  err(FATAL,"STP: cannot open ws_fdw/ws_fdr\n");
	
	/* send authorization */
	sprintf(line,"%s %s %s\n","STP",PASSWD,VERSION);
	ws_putline(line);
	fflush(ws_fdw);
	
	/* get connection acknowledgement */
	ws_getline(line,MAXLINE);
	if(strcmp(line,"CONNECTED\n") != 0)
	  err(FATAL,"STP: Connection rejected by waveserver - %s\n", line);

	fprintf(stderr,"STP: Connected to %s\n",address[iserv].addr);
	
	/* send a binary sample so the server can establish byte order */
	two= 2;
	ws_putdata(&two,4);
	fflush(ws_fdw);
	
	/* receiver possible message from server */
	receive_data();
    }

}

void
close_waveserver()
   {
	fclose(ws_fdr);
	fclose(ws_fdw);
	close(ws_sockfd);
   }

void
ws_putline(char *str)
   {
	fwrite(str,1,strlen(str),ws_fdw);
	fflush(ws_fdw);
   }

int
ws_getline(char *str, int maxlen)
   {
	char *p, *fgets();
	p= fgets(str,maxlen,ws_fdr);
	if(p == NULL) return(EOF);
	return(strlen(p));
   }

int
ws_getdata(int *buf, int ndata)
   {
	int n;
	n= fread(buf,1,ndata,ws_fdr);
	return(n);
   }

int
ws_putdata(int *buf, int ndata)
   {
	int n;
	n= fwrite(buf,1,ndata,ws_fdw);
	return(n);
   }

#include	<sys/types.h>
#include	<sys/stat.h>

int makedirectory(char *dir)
   {
	struct stat sb;

	if(stat(dir,&sb) >= 0 )
	   {
		if((sb.st_mode & S_IFDIR) == 0)
		    {
			err(WARN,"directory %s exits as a file\n",dir);
			return(-1);
		   }
	   }
	 else
	   {
		if( mkdir(dir,0775) < 0)
		   {
			err(WARN,"cannot create directory %s\n",dir);
			return(-1);
		   }
	   }
	return(1);
   }

void
usage()
   {
	fprintf(stderr,"stp [-v] [-d directory] [-a addr] [-p port] wave_server\n");
	fprintf(stderr,"\t -v => verbose\n");
	fprintf(stderr,"\t -d directory  => specify directory for output\n");
	fprintf(stderr,"\t -a addr  => numeric internet address of server\n");
	fprintf(stderr,"\t -a port  => numeric port number for connection\n");
	fprintf(stderr,"\t waveserver (i.e. scedc) wave server to use\n");
	fprintf(stderr,"  At the prompt STP>, type help\n");
   }

err(int mode, char *fmt, int a1, int a2, int a3, int a4)
   {
	fprintf(stderr,fmt,a1,a2,a3,a4);
	if(mode == FATAL) exit(-1);
   }

report(char *fmt, int a1, int a2, int a3, int a4)
   {
	if(verbose == 0) return;
	fprintf(stderr,fmt,a1,a2,a3,a4);
	if(cpyout) fprintf(cpyout,fmt,a1,a2,a3,a4);
   }


void
make_upper_case(char *s)
   {
	for( ; *s != '\0'; s++)
		if(*s >= 'a' && *s <= 'z') *s -= ('a' - 'A');
   }
