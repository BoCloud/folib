package com.veadan.folib.domain.huggingface.index;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;

import com.veadan.folib.domain.huggingface.index.processors.*;
import com.veadan.folib.domain.huggingface.model.CardData;
import com.veadan.folib.domain.huggingface.model.RevisionData;
import lombok.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class MlModelIndexUtils {

    private static final Logger log = LoggerFactory.getLogger(MlModelIndexUtils.class);

    public static final int MAX_STREAM_SIZE = 52428800;

    private MlModelIndexUtils() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    private static final List<LineProcessor> processors = List.of(new LicenseProcessor(), new LibraryNameProcessor(), new ModelNameProcessor(), new TagsProcessor(), new LanguagesProcessor());

    public static RevisionData parseReadme(InputStream stream) throws IOException {
        RevisionData ret = new RevisionData();
        ret.setCardData(new CardData());
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(stream));
        validateStreamNotExceedingSize(bufferedReader);
        MarkdownReader markdownReader = new MarkdownReader(bufferedReader);
        String line;
        while ((line = markdownReader.readLine()) != null) {
            for (LineProcessor processor : processors) {
                processor.process(line, ret, markdownReader);
            }
        }
        return ret;
    }

    private static void validateStreamNotExceedingSize(BufferedReader bis) throws RuntimeException, IOException {
        bis.mark(104857600);
        long totalBytesRead = 0L;
        char[] buffer = new char[1024];
        int bytesRead;
        while ((bytesRead = bis.read(buffer)) != -1) {
            totalBytesRead += bytesRead;
            if (totalBytesRead > 52428800L) {
                throw new RuntimeException("README size exceeds the maximum allowed of 50MB");
            }
        }
        bis.reset();
    }
}

