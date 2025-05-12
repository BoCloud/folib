/**
功能：搜索结果展示
作者：张佳宁
日期：
**/
<template>
    <div class='search-result'>
        <div class="search_list" v-for="item,i in results" :key="i">
            <a :href="item.path">{{ item.title }}</a>
        </div>
    </div>
</template>

<script>
export default {
    props:['results'],
    data() {
        return {
        }
    },
    mounted() {

    },
    methods:{

    }
}
</script>
<style lang="scss" scoped>
    .search-result{
        padding: 20px;
        box-sizing: border-box;
        position: absolute;
        text-align: left;
        max-height: 300px;
        overflow-y:auto;
        width: 100%;
        top: 62px;
        border-radius: 8px;
        background: rgba(255, 255, 255, 0.5); 
        z-index: 99;
        backdrop-filter: blur(20px);
        box-shadow: 0px 8px 14px 0px rgba(214, 217, 226, 0.5);
        
        .search_list{
            height: 30px;
            color: #3f424a;
            padding-left: 15px;
            border-radius: 4px;
            
            a{
                cursor: pointer;
                line-height: 30px;
            }

            &:hover{
                background: #fff;
            }
        }
    }
</style>