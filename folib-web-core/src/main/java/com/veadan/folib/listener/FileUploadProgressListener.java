package com.veadan.folib.listener;

import org.apache.commons.fileupload.ProgressListener;
import org.springframework.stereotype.Component;
import javax.servlet.http.HttpSession;

/**
 * @author leipenghui
 * @date 2023/2/12
 **/
@Component
public class FileUploadProgressListener implements ProgressListener {

    private HttpSession session;

    public void setSession(HttpSession session) {
        this.session = session;
        session.setAttribute("upload_percent", 0);
    }

    @Override
    public void update(long pBytesRead, long pContentLength, int pItems) {
        int percent = (int) (pBytesRead * 100.0 / pContentLength);
        session.setAttribute("upload_percent", percent);
    }
}
