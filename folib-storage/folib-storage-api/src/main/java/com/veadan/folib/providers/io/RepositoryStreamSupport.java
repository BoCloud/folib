package com.veadan.folib.providers.io;

import cn.hutool.core.date.StopWatch;
import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.io.*;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.CommonUtils;
import org.apache.commons.io.input.CountingInputStream;
import org.apache.commons.io.input.ProxyInputStream;
import org.apache.commons.io.output.CountingOutputStream;
import org.apache.commons.io.output.ProxyOutputStream;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.support.DefaultTransactionDefinition;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * @author xuxinping
 */
public class RepositoryStreamSupport {

    private static final Logger logger = LoggerFactory.getLogger(RepositoryStreamSupport.class);

    private RepositoryStreamContext ctx = new RepositoryStreamContext();

    protected final RepositoryPathLock repositoryPathLock;

    protected final RepositoryStreamCallback callback;

    private final PlatformTransactionManager transactionManager;

    public RepositoryStreamSupport(RepositoryPathLock repositoryPathLock,
                                   RepositoryStreamCallback callback,
                                   PlatformTransactionManager transactionManager) {
        this.repositoryPathLock = repositoryPathLock;
        this.callback = callback;
        this.transactionManager = transactionManager;
    }

    protected void initContext(RepositoryStreamContext ctx) {
        this.ctx = ctx;
    }

    protected RepositoryStreamContext getContext() {
        return ctx;
    }

    private void clearContext() {
        ctx = null;
    }

    private void handlerLock()
            throws IOException {
        RepositoryStreamContext ctx = getContext();
        if (ctx.isOpened()) {
            return;
        }
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        try {
            logger.debug("Locking [{}].", repositoryPath);
            if (repositoryPathLock.lock(repositoryPath)) {
                ctx.setLocked(true);
                logger.debug("Locked [{}].", repositoryPath);
                try {
                    open(repositoryPath);
                } catch (Exception ex) {
                    unLock();
                    logger.error("Unlocked [{}] repositoryPath [{}] lock error [{}]", this.getClass().getSimpleName(), repositoryPath, ExceptionUtils.getStackTrace(ex));
                    throw ex;
                }
            } else {
                logger.warn("[{}] repositoryPath [{}] was not get lock", this.getClass().getSimpleName(), repositoryPath);
            }
        } catch (Exception ex) {
            logger.error("RepositoryPath [{}] lock error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
            throw new IOException(ex.getMessage());
        }
    }

    private void open(RepositoryPath repositoryPath) throws IOException {
        if (ctx instanceof RepositoryStreamWriteContext) {
            TransactionStatus transaction = transactionManager.getTransaction(new DefaultTransactionDefinition(
                    Propagation.REQUIRED.value()));
            ctx.setTransaction(transaction);
        }

        ctx.setArtifactExists(RepositoryFiles.artifactExists(repositoryPath));

        ctx.setOpened(true);
    }

    protected void close()
            throws IOException {
        long startTime = System.currentTimeMillis();
        String path = ctx.getPath().toString();
        try {
            RepositoryStreamContext ctx = getContext();
            if (!ctx.isOpened()) {
                return;
            }
            RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
            TransactionStatus transaction = ctx.getTransaction();
            if (transaction != null && (transaction.isRollbackOnly() || !transaction.isCompleted())) {
                logger.warn("Rollback [{}]", getContext().getPath());
                transactionManager.rollback(transaction);
                logger.warn("Rolled back [{}]", getContext().getPath());
                if (RepositoryTypeEnum.PROXY.getType().equalsIgnoreCase(repositoryPath.getRepository().getType()) && Files.exists(repositoryPath)) {
                    logger.info("Rollback back file path [{}] size [{}] store size [{}]", getContext().getPath(), repositoryPath.getSize(), Files.size(repositoryPath));
                    RepositoryFiles.delete(repositoryPath);
                    logger.warn("Rolled back file path [{}]", getContext().getPath());
                }
            }
        } finally {
            if (Objects.nonNull(ctx.getLocked())) {
                unLock();
                logger.info("Unlocked [{}].", path);
            }
            clearContext();
            logger.info("Close [{}] take time [{}] ms", path, System.currentTimeMillis() - startTime);
        }
    }

    protected void commit() throws IOException {
        callback.commit((RepositoryStreamWriteContext) getContext());
    }

    protected void commitStoreIndex() throws IOException {
        callback.commitStoreIndex((RepositoryStreamReadContext) getContext());
    }

    public class RepositoryOutputStream extends ProxyOutputStream {
        protected RepositoryOutputStream(Path path,
                                         OutputStream out) throws IOException {
            super(new CountingOutputStream(out));

            RepositoryStreamWriteContext ctx = new RepositoryStreamWriteContext();
            ctx.setStream(this);
            ctx.setPath(path);
            initContext(ctx);

            try {
                handlerLock();

                // Force init LazyInputStream
                StreamUtils.findSource(LazyOutputStream.class, out).init();
            } catch (Exception e) {
                close();
                throw new IOException(e);
            }
        }

        @Override
        protected void beforeWrite(int n)
                throws IOException {
            if (((CountingOutputStream) out).getByteCount() == 0) {
                callback.onBeforeWrite((RepositoryStreamWriteContext) ctx);
            }
        }


        @Override
        public void flush()
                throws IOException {
            StopWatch stopWatch = new StopWatch("Flush " + getContext().getPath());
            String path = ctx.getPath().toString();
            String className = this.getClass().getSimpleName();
            if (logger.isDebugEnabled()) {
                logger.debug("Flushing [{}]", path);
            }
            stopWatch.start("super.flush");
            super.flush();
            stopWatch.stop();
            if(logger.isDebugEnabled()) {
                logger.debug("Flushed [{}]", path);
            }
            TransactionStatus transaction = ctx.getTransaction();
            if (transaction != null && !transaction.isRollbackOnly()) {
                logger.info("Commit [{}]", path);
                try {
                    stopWatch.start("db commit");
                    RepositoryStreamSupport.this.commit();
                    stopWatch.stop();
                    stopWatch.start("transaction commit");
                    transactionManager.commit(transaction);
                    stopWatch.stop();
                } catch (Exception ex) {
                    String realMessage = CommonUtils.getRealMessage(ex);
                    logger.warn("[{}] [{}] flush error [{}]",
                            className, path, realMessage);
                    if (CommonUtils.catchException(realMessage)) {
                        logger.warn("[{}] [{}] flush catch error",
                                className, path);
                        return;
                    }
                    throw ex;
                }
            } else {
                logger.warn("Skip commit [{}]", getContext().getPath());
            }
            logger.info("【Flush】 [{}] completed, total stats: \n {}", path,stopWatch.prettyPrint(TimeUnit.MILLISECONDS));
        }

        @Override
        public void close()
                throws IOException {
            try {
                StopWatch stopWatch = new StopWatch("Close Step");
                stopWatch.start("super.close");
                super.close();
                stopWatch.stop();
                stopWatch.start("OnAfterWrite");
                if (((CountingOutputStream) out).getByteCount() > 0) {
                    callback.onAfterWrite((RepositoryStreamWriteContext) ctx);
                }
                stopWatch.stop();
                logger.info("【Close】 [{}] completed ,total stats:\n {}", getContext().getPath(),stopWatch.prettyPrint(TimeUnit.MILLISECONDS));
            } catch (Exception e) {
                logger.error("Failed to close [{}].", getContext().getPath(), e);

                throw e;
            } finally {
                RepositoryStreamSupport.this.close();
            }
        }

    }

    public class RepositoryInputStream
            extends ProxyInputStream {

        protected RepositoryInputStream(Path path,
                                        InputStream in) throws IOException {
            super(new CountingInputStream(in));

            RepositoryStreamReadContext ctx = new RepositoryStreamReadContext();
            ctx.setPath(path);
            ctx.setStream(this);
            initContext(ctx);

            try {
                RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
                open(repositoryPath);

                //Check that artifact exists.
                if (!ctx.getArtifactExists()) {
                    logger.info("The path [{}] does not exist!", path);

                    throw new ArtifactNotFoundException(path.toUri());
                }

                // Force init LazyInputStream
                StreamUtils.findSource(LazyInputStream.class, in).init();
            } catch (Exception e) {
                close();
                throw new IOException(e);
            }
        }

        @Override
        protected void beforeRead(int n)
                throws IOException {
            if (((CountingInputStream) in).getByteCount() == 0) {
                callback.onBeforeRead((RepositoryStreamReadContext) ctx);
            }
        }

        @Override
        public void close()
                throws IOException {
            try {
                super.close();
                if (((CountingInputStream) in).getByteCount() > 0) {
                    callback.onAfterRead((RepositoryStreamReadContext) ctx);
                }
            } finally {
                RepositoryStreamSupport.this.close();
            }
        }

    }

    public class RepositoryStoreIndexInputStream
            extends ProxyInputStream {

        protected RepositoryStoreIndexInputStream(Path path,
                                                  InputStream in) throws IOException {
            super(new CountingInputStream(in));

            RepositoryStreamReadContext ctx = new RepositoryStreamReadContext();
            ctx.setPath(path);
            ctx.setStream(this);
            initContext(ctx);
            TransactionStatus transaction = transactionManager.getTransaction(new DefaultTransactionDefinition(
                    Propagation.REQUIRED.value()));
            ctx.setTransaction(transaction);
            try {
                handlerLock();
                // Force init LazyInputStream
                StreamUtils.findSource(LazyInputStream.class, in).init();
            } catch (Exception e) {
                close();
                throw new IOException(e);
            }
        }

        public void commitStoreIndex()
                throws IOException {
            TransactionStatus transaction = ctx.getTransaction();
            if (transaction != null && !transaction.isRollbackOnly()) {
                logger.debug("Commit [{}]", getContext().getPath());
                RepositoryStreamSupport.this.commitStoreIndex();
                transactionManager.commit(transaction);
                logger.debug("Commited [{}]", getContext().getPath());
                callback.onStoreIndexAfter((RepositoryStreamReadContext) ctx);
            } else {
                logger.warn("Skip commit [{}]", getContext().getPath());
            }
        }

        @Override
        public void close()
                throws IOException {
            try {
                Path path = getContext().getPath();
                logger.debug("[{}] start close", path);
                super.close();
                logger.debug("[{}] end close", path);
            } finally {
                Path path = getContext().getPath();
                logger.debug("[{}] finally start close", path);
                RepositoryStreamSupport.this.close();
                logger.debug("[{}] finally end close", path);
            }
        }

    }

    private void unLock() {
        Boolean locked = ctx.getLocked();
        if (Boolean.TRUE.equals(locked)) {
            repositoryPathLock.unLock((RepositoryPath) ctx.getPath());
        }
    }

}
