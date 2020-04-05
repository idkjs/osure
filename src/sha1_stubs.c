/* Hashing stubs. */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <openssl/sha.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>

#define BUFFER_SIZE (128*1024)

/* Hash the contents of the file, returning the sha1 sum as an array
 * of bytes. */
value hash_file(value fname)
{
	CAMLparam1(fname);
	CAMLlocal1(result);
	char *name;
	char *buf;
	int fd;
	int rc;
	SHA_CTX ctx;
	char hash[SHA_DIGEST_LENGTH];

	/* Copy the name to the C heap. */
	name = caml_stat_strdup(String_val(fname));

	buf = caml_stat_alloc(BUFFER_SIZE);

	/* TODO: It might not be worth releasing the lock if the file
	 * is small, but then again, releasing the lock is probably
	 * less work than figuring out the size of the file.  We
	 * could, perhaps, try hashing one block, and if that isn't
	 * the end of the file, then release the lock. */
	caml_release_runtime_system();

	SHA1_Init(&ctx);
	fd = open(name, O_RDONLY | __O_NOATIME);
	if (fd < 0 && errno == EPERM) {
		fd = open(name, O_RDONLY);
	}

	if (fd < 0) {
		goto fail1;
	}

	while (1) {
		rc = read(fd, buf, BUFFER_SIZE);
		if (rc == 0) {
			break;
		}
		if (rc < 0) {
			printf("Hash failure on %s (%d)\n", name, errno);
			fflush(stdout);
			goto out2;
		}

		SHA1_Update(&ctx, buf, rc);
	}

	SHA1_Final(hash, &ctx);

out2:
	close(fd);
fail1:
	caml_acquire_runtime_system();
	caml_stat_free(buf);
	caml_stat_free(name);

	if (rc < 0 || fd < 0) {
		caml_failwith("Error hashing file");
	}

	/* TODO: Handle errors here. */
	result = caml_alloc_string(SHA_DIGEST_LENGTH);
	memcpy(Bytes_val(result), hash, SHA_DIGEST_LENGTH);
	CAMLreturn(result);
}
