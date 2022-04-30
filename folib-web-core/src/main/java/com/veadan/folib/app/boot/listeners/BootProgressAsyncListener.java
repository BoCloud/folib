package com.veadan.folib.app.boot.listeners;

import javax.servlet.AsyncEvent;
import javax.servlet.AsyncListener;
import java.io.IOException;

import io.reactivex.disposables.Disposable;

public class BootProgressAsyncListener
        implements AsyncListener
{

    private volatile Disposable disposable;

    public BootProgressAsyncListener(Disposable disposable)
    {
        this.disposable = disposable;
    }

    private void dispose()
    {
        if (disposable != null && !disposable.isDisposed())
        {
            disposable.dispose();
        }
    }

    @Override
    public void onComplete(AsyncEvent event)
            throws IOException
    {
        this.dispose();
    }

    @Override
    public void onTimeout(AsyncEvent event)
            throws IOException
    {
        this.dispose();
    }

    @Override
    public void onError(AsyncEvent event)
            throws IOException
    {
        this.dispose();
    }

    @Override
    public void onStartAsync(AsyncEvent event)
            throws IOException
    {
    }
}
