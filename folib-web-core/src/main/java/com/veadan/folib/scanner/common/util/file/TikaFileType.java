package com.veadan.folib.scanner.common.util.file;

import com.veadan.folib.scanner.common.exception.BusinessException;
import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.apache.tika.metadata.HttpHeaders;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.mime.MediaType;
import org.apache.tika.mime.MimeType;
import org.apache.tika.mime.MimeTypeException;
import org.apache.tika.mime.MimeTypes;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.Parser;
import org.xml.sax.helpers.DefaultHandler;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;

public class TikaFileType {

    public static  String getFileType(File file){

        Tika tika = new Tika();

        String contentType = null;
        try {
            contentType = tika.detect(file);
        } catch (IOException e) {
            throw new BusinessException("文件解析失败");
        }

        MimeTypes allTypes = MimeTypes.getDefaultMimeTypes();

        MimeType mime = null;
        try {
            mime = allTypes.forName(contentType);
        } catch (MimeTypeException e) {
            throw new BusinessException("文件解析失败");
        }
        return mime.getExtension();

    }
//    public static String getMimeType(File file) {
//        if (file.isDirectory()) {
//            return "the target is a directory";
//        }
//
//        AutoDetectParser parser = new AutoDetectParser();
//        parser.setParsers(new HashMap<MediaType, Parser>());
//
//        Metadata metadata = new Metadata();
//        metadata.add(TikaMetadataKeys.RESOURCE_NAME_KEY, file.getName());
//
//        InputStream stream;
//        try {
//            stream = new FileInputStream(file);
//            parser.parse(stream, new DefaultHandler(), metadata, new ParseContext());
//            stream.close();
//        } catch (TikaException | IOException e) {
//            e.printStackTrace();
//        } catch (org.xml.sax.SAXException e) {
//            throw new RuntimeException(e);
//        }
//
//        return metadata.get(HttpHeaders.CONTENT_TYPE);
//    }
}
