package com.veadan.folib.event.repository;

/**
 * @author Veadan
 */
public enum RepositoryEventTypeEnum
{

    /**
     * Occurs when a repository has been created.
     */
    EVENT_REPOSITORY_CREATED(1),

    /**
     * Occurs when a repository has been deleted.
     */
    EVENT_REPOSITORY_DELETED(2),

    /**
     * Occurs when a repository has been put in service.
     */
    EVENT_REPOSITORY_PUT_IN_SERVICE(3),

    /**
     * Occurs when a repository has been put out of service.
     */
    EVENT_REPOSITORY_PUT_OUT_OF_SERVICE(4),

    /**
     * Occurs when a proxy repository's remote host has become unreachable.
     *
     * TODO: Not yet implemented.
     */
    EVENT_REPOSITORY_REMOTE_UNAVAILABLE(5),

    /**
     * Occurs when a proxy repository's remote host has become reachable.
     *
     * TODO: Not yet implemented.
     */
    EVENT_REPOSITORY_REMOTE_AVAILABLE(6),

    /**
     * Occurs when a repository's trash has been removed.
     */
    EVENT_REPOSITORY_EMTPY_TRASH(7),

    /**
     * Occurs when all the trash for repositories has been deleted.
     */
    EVENT_REPOSITORY_EMTPY_TRASH_FOR_ALL_REPOSITORIES(8),

    /**
     * Occurs when a repository's trash has been deleted.
     */
    EVENT_REPOSITORY_UNDELETE_TRASH(9),

    /**
     * Occurs when all the trash for repositories has been undeleted.
     */
    EVENT_REPOSITORY_UNDELETE_TRASH_FOR_ALL_REPOSITORIES(10),

    EVENT_REPOSITORY_DELETE_TO_CRON_JOB_DELETED(11),

    EVENT_REPOSITORY_DELETE_ALL_TO_CRON_JOB_DELETED(12);



    private int type;


    RepositoryEventTypeEnum(int type)
    {
        this.type = type;
    }

    public int getType()
    {
        return type;
    }

}
