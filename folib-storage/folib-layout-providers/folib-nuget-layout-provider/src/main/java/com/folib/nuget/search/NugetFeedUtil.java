package com.folib.nuget.search;

import com.folib.nuget.odata.feed.Entry;
import com.folib.nuget.odata.feed.Feed;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;



public class NugetFeedUtil {
    public static Feed mergeFeeds(List<Feed> feeds, String baseUrl, String title) {
        List<Entry> entries = new ArrayList<>();
        for (Feed feed : feeds) {
            if (feed != null && feed.getEntries() != null) {
                entries.addAll(feed.getEntries());
            }
        }
        return new Feed(baseUrl, entries, title);
    }

//    public static Feed page(Feed feed, int pageSize, int pageIndex) {
//        return null;
//    }

    public static Feed buildFeed(List<Entry> entries, String v2BaseUrl, String title) {
        return new Feed(v2BaseUrl, entries, title);
    }

    public static Feed buildFeed(Map<String, Map<String, Entry>> searchedPackages, String v2BaseUrl, String title) {
        List<Entry> entries = new ArrayList<>();
        for (Map.Entry<String, Map<String, Entry>> packageEntry : searchedPackages.entrySet()) {
            Map<String, Entry> versionMap = packageEntry.getValue();
            for (Map.Entry<String, Entry> versionEntry : versionMap.entrySet()) {
                Entry entry = versionEntry.getValue();
                entries.add(entry);
            }
        }
        return new Feed(v2BaseUrl, entries, title);
    }

}
