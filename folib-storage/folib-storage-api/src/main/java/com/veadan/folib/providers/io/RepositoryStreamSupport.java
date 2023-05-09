package com.veadan.folib.providers.io;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.enums.LockTypeEnum;
import com.veadan.folib.io.*;
import com.veadan.folib.services.RedLockService;
import com.veadan.folib.util.CommonUtils;
import org.apache.commons.io.input.CountingInputStream;
import org.apache.commons.io.input.ProxyInputStream;
import org.apache.commons.io.output.CountingOutputStream;
import org.apache.commons.io.output.ProxyOutputStream;
import org.apache.commons.lang3.StringUtils;
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
import java.nio.file.Path;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;

/**
 * @author xuxinping
 */
public class RepositoryStreamSupport {

    private static final Logger logger = LoggerFactory.getLogger(RepositoryStreamSupport.class);

    private RepositoryStreamContext ctx = new RepositoryStreamContext();

    protected final ReadWriteLock lockSource;

    protected final RedLockService redLockService;

    protected final RepositoryStreamCallback callback;

    private final PlatformTransactionManager transactionManager;

    private String lockType;

    public RepositoryStreamSupport(ReadWriteLock lockSource,
                                   RepositoryStreamCallback callback,
                                   PlatformTransactionManager transactionManager) {
        this.lockSource = lockSource;
        this.redLockService = SpringUtil.getBean(RedLockService.class);
        this.callback = callback;
        this.transactionManager = transactionManager;
        String key = "lockType";
        String value = System.getProperty(key);
        this.lockType = LockTypeEnum.queryType(value);
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
        String lockKey = repositoryPath.toString();
        long waitTimeout = 30000L;
        if (LockTypeEnum.LOCAL.getType().equals(lockType)) {
            logger.info("Locking [{}].", repositoryPath);
            //本地锁
            Lock lock = lockSource.writeLock();
            ctx.setLock(lock);
            try {
                if (lock.tryLock(waitTimeout, TimeUnit.MILLISECONDS)) {
                    logger.info("Locked [{}].", repositoryPath);
                    try {
                        open(repositoryPath);
                    } catch (Exception ex) {
                        lock.unlock();
                        logger.error("Unlocked [{}] repositoryPath：{} lock error：{}", this.getClass().getSimpleName(), repositoryPath, ExceptionUtils.getStackTrace(ex));
                        throw ex;
                    }
                } else {
                    logger.warn("[{}] repositoryPath：{} was not get lock", this.getClass().getSimpleName(), repositoryPath);
                }
            } catch (Exception ex) {
                logger.error("[{}] repositoryPath：{} lock error：{}", this.getClass().getSimpleName(), repositoryPath, ExceptionUtils.getStackTrace(ex));
                throw new IOException(ex.getMessage());
            }
        } else if (LockTypeEnum.DISTRIBUTION.getType().equals(lockType)) {
            if (RepositoryFiles.isArtifact(repositoryPath)) {
                ArtifactIdGroupEntity artifactIdGroupEntity = new ArtifactIdGroupEntity(repositoryPath.getStorageId(),
                        repositoryPath.getRepositoryId(),
                        RepositoryFiles.readCoordinates(repositoryPath).getId());
                lockKey = artifactIdGroupEntity.getUuid();
            }
            ctx.setLockKey(lockKey);
            logger.info("Locking [{}] by distribution lockKey {}.", repositoryPath, lockKey);
            //分布式锁
            try {
                if (redLockService.tryLockTimeout(lockKey, waitTimeout)) {
                    logger.info("Locked [{}] by distribution lockKey {}.", repositoryPath, lockKey);
                    try {
                        open(repositoryPath);
                    } catch (Exception ex) {
                        redLockService.unLock(lockKey);
                        logger.error("Unlocked [{}] repositoryPath：{} distribution lockKey {} lock error：{}", this.getClass().getSimpleName(), repositoryPath, lockKey, ExceptionUtils.getStackTrace(ex));
                        throw ex;
                    }
                } else {
                    logger.warn("[{}] repositoryPath：{} was not get distribution lock", this.getClass().getSimpleName(), lockKey);
                }
            } catch (Exception ex) {
                logger.error("[{}] repositoryPath：{} distribution lockKey {} lock error：{}", this.getClass().getSimpleName(), repositoryPath, lockKey, ExceptionUtils.getStackTrace(ex));
                throw new IOException(ex.getMessage());
            }
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
            TransactionStatus transaction = ctx.getTransaction();
            if (transaction != null && (transaction.isRollbackOnly() || !transaction.isCompleted())) {
                logger.info("Rollback [{}]", getContext().getPath());
                transactionManager.rollback(transaction);
                logger.info("Rollbedack [{}]", getContext().getPath());
            }
        } finally {
            if (LockTypeEnum.LOCAL.getType().equals(lockType)) {
                if (Objects.nonNull(ctx.getLock())) {
                    ctx.getLock().unlock();
                    logger.info("Unlocked [{}].", path);
                }
                clearContext();
            } else if (LockTypeEnum.DISTRIBUTION.getType().equals(lockType)) {
                if (StringUtils.isNotBlank(ctx.getLockKey())) {
                    redLockService.unLock(ctx.getLockKey());
                    logger.info("Unlocked lockKey [{}] by distribution.", ctx.getLockKey());
                }
                clearContext();
            }
            logger.info("[{}] close {} take time：{} ms", this.getClass().getSimpleName(), path, System.currentTimeMillis() - startTime);
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
            logger.info("Flushing [{}]", getContext().getPath());

            super.flush();

            logger.info("Flushed [{}]", getContext().getPath());

            TransactionStatus transaction = ctx.getTransaction();
            if (transaction != null && !transaction.isRollbackOnly()) {
                logger.info("Commit [{}]", getContext().getPath());
                try {
                    RepositoryStreamSupport.this.commit();
                    transactionManager.commit(transaction);
                    logger.info("Commited [{}]", getContext().getPath());
                } catch (Exception ex) {
                    String realMessage = CommonUtils.getRealMessage(ex);
                    logger.warn("[{}] [{}] flush error [{}]",
                            this.getClass().getSimpleName(), getContext().getPath(), realMessage);
                    if (CommonUtils.catchException(realMessage)) {
                        logger.warn("[{}] [{}] flush catch error",
                                this.getClass().getSimpleName(), getContext().getPath());
                        return;
                    }
                    throw ex;
                }
            } else {
                logger.info("Skip commit [{}]", getContext().getPath());
            }
        }

        @Override
        public void close()
                throws IOException {
            try {
                super.close();
                if (((CountingOutputStream) out).getByteCount() > 0) {
                    callback.onAfterWrite((RepositoryStreamWriteContext) ctx);
                }
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
                logger.info("Commit [{}]", getContext().getPath());
                RepositoryStreamSupport.this.commitStoreIndex();
                transactionManager.commit(transaction);
                logger.info("Commited [{}]", getContext().getPath());
                callback.onStoreIndexAfter((RepositoryStreamReadContext) ctx);
            } else {
                logger.info("Skip commit [{}]", getContext().getPath());
            }
        }

        @Override
        public void close()
                throws IOException {
            try {
                Path path = getContext().getPath();
                logger.info("{} start close", path);
                super.close();
                logger.info("{} end close", path);
            } finally {
                Path path = getContext().getPath();
                logger.info("{} finally start close", path);
                RepositoryStreamSupport.this.close();
                logger.info("{} finally end close", path);
            }
        }

    }

}
