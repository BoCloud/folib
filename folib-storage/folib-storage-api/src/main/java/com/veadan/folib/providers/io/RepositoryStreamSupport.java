package com.veadan.folib.providers.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;

import org.apache.commons.io.input.CountingInputStream;
import org.apache.commons.io.input.ProxyInputStream;
import org.apache.commons.io.output.CountingOutputStream;
import org.apache.commons.io.output.ProxyOutputStream;
import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.io.LazyInputStream;
import com.veadan.folib.io.LazyOutputStream;
import com.veadan.folib.io.RepositoryStreamCallback;
import com.veadan.folib.io.RepositoryStreamContext;
import com.veadan.folib.io.RepositoryStreamReadContext;
import com.veadan.folib.io.RepositoryStreamWriteContext;
import com.veadan.folib.io.StreamUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.support.DefaultTransactionDefinition;

/**
 * @author sbespalov
 *
 */
public class RepositoryStreamSupport
{
    
    private static final Logger logger = LoggerFactory.getLogger(RepositoryStreamSupport.class);

    private RepositoryStreamContext ctx = new RepositoryStreamContext();

    protected final ReadWriteLock lockSource;
    
    protected final RepositoryStreamCallback callback;
    
    private final PlatformTransactionManager transactionManager;
    
    
    public RepositoryStreamSupport(ReadWriteLock lockSource,
                                   RepositoryStreamCallback callback,
                                   PlatformTransactionManager transactionManager)
    {
        this.lockSource = lockSource;
        this.callback = callback;
        this.transactionManager = transactionManager;
    }

    protected void initContext(RepositoryStreamContext ctx)
    {
        this.ctx = ctx;
    }

    protected RepositoryStreamContext getContext()
    {
        return ctx;
    }

    private void clearContext()
    {
        ctx = null;
    }

    private void open()
        throws IOException
    {
        RepositoryStreamContext ctx = getContext();
        if (ctx.isOpened())
        {
            return;
        }

        RepositoryPath path = (RepositoryPath) ctx.getPath();
        
        logger.debug("Locking [{}].", path);
        
        //TODO: ArtifactManagementServiceImplTest.testConcurrentReadWrite
        //Lock lock = ctx instanceof RepositoryStreamWriteContext ? lockSource.writeLock() : lockSource.readLock();
        Lock lock = lockSource.writeLock();
        ctx.setLock(lock);
        lock.lock();
        logger.debug("Locked [{}].", path);
        if (ctx instanceof RepositoryStreamWriteContext)
        {
            TransactionStatus transaction = transactionManager.getTransaction(new DefaultTransactionDefinition(
                    Propagation.REQUIRED.value()));
            ctx.setTransaction(transaction);
        }
        
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        ctx.setArtifactExists(RepositoryFiles.artifactExists(repositoryPath));
        
        ctx.setOpened(true);
    }

    protected void close()
        throws IOException
    {
        RepositoryStreamContext ctx = getContext();
        if (!ctx.isOpened())
        {
            return;
        }

        try
        {
            TransactionStatus transaction = ctx.getTransaction();
            if (transaction != null && (transaction.isRollbackOnly() || !transaction.isCompleted()))
            {
                logger.info("Rollback [{}]", getContext().getPath());
                transactionManager.rollback(transaction);
                logger.info("Rollbedack [{}]", getContext().getPath());
            }
        }
        finally
        {
            ctx.getLock().unlock();
            logger.debug("Unlocked [{}].", ctx.getPath());
        }
        
        clearContext();
    }

    protected void commit() throws IOException
    {
        callback.commit((RepositoryStreamWriteContext) getContext());
    }

    public class RepositoryOutputStream extends ProxyOutputStream
    {
        protected RepositoryOutputStream(Path path,
                                         OutputStream out) throws IOException
        {
            super(new CountingOutputStream(out));

            RepositoryStreamWriteContext ctx = new RepositoryStreamWriteContext();
            ctx.setStream(this);
            ctx.setPath(path);
            initContext(ctx);
            
            try
            {
                open();
                
                // Force init LazyInputStream
                StreamUtils.findSource(LazyOutputStream.class, out).init();;
            }
            catch (Exception e)
            {
                close();
                throw new IOException(e);
            }
        }
        
        @Override
        protected void beforeWrite(int n)
            throws IOException
        {
            if (((CountingOutputStream) out).getByteCount() == 0)
            {
                callback.onBeforeWrite((RepositoryStreamWriteContext) ctx);
            }
        }


        public void flush()
            throws IOException
        {
            logger.debug("Flushing [{}]", getContext().getPath());
            
            super.flush();
            
            logger.debug("Flushed [{}]", getContext().getPath());
            
            TransactionStatus transaction = ctx.getTransaction();
            if (transaction != null && !transaction.isRollbackOnly())
            {
                logger.debug("Commit [{}]", getContext().getPath());
                RepositoryStreamSupport.this.commit();
                transactionManager.commit(transaction);
                logger.debug("Commited [{}]", getContext().getPath());
            }
            else
            {
                logger.debug("Skip commit [{}]", getContext().getPath());
            }
        }

        @Override
        public void close()
            throws IOException
        {
            try
            {
                super.close();
                if (((CountingOutputStream) out).getByteCount() > 0) 
                {
                    callback.onAfterWrite((RepositoryStreamWriteContext) ctx);
                }
            }
            catch (Exception e) 
            {
                logger.error("Failed to close [{}].", getContext().getPath(), e);
                
                throw e;
            }
            finally
            {
                RepositoryStreamSupport.this.close();
            }
        }

    }

    public class RepositoryInputStream
            extends ProxyInputStream
    {

        protected RepositoryInputStream(Path path,
                                        InputStream in) throws IOException
        {
            super(new CountingInputStream(in));
            
            RepositoryStreamReadContext ctx = new RepositoryStreamReadContext();
            ctx.setPath(path);
            ctx.setStream(this);
            initContext(ctx);
            
            try
            {
                open();
                
                //Check that artifact exists.
                if (!ctx.getArtifactExists()) 
                {
                    logger.debug("The path [{}] does not exist!", path);
                    
                    throw new ArtifactNotFoundException(path.toUri());
                }
                
                // Force init LazyInputStream
                StreamUtils.findSource(LazyInputStream.class, in).init();
            }
            catch (Exception e)
            {
                close();
                throw new IOException(e);
            }
        }

        @Override
        protected void beforeRead(int n)
            throws IOException
        {
            if (((CountingInputStream) in).getByteCount() == 0)
            {
                callback.onBeforeRead((RepositoryStreamReadContext) ctx);
            }
        }

        @Override
        public void close()
            throws IOException
        {
            try
            {
                super.close();
                if (((CountingInputStream) in).getByteCount() > 0) 
                {
                    callback.onAfterRead((RepositoryStreamReadContext) ctx);
                }
            } 
            finally
            {
                RepositoryStreamSupport.this.close();
            }
        }

    }

}
