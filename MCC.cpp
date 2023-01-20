
// Author: Wenyu Zhang
// Data: Dec 10th 2022

#include <iostream>
#include <vector> //std::vector
#include <numeric>
#include <algorithm>
#include <map> // std::map
#include <iostream>
#include "bits-stdc++.h"// vector.erase
#include <stack> // std::stack

using namespace std;

typedef std::vector< std::vector<int> > Matrix;

class Schedule{
public:
    Schedule(vector<int> input_task, int N, Matrix &input_graph,
             int K, Matrix &l_table,int T_s, int T_c, int T_r, vector<float> powers){
                 // using this to refer class members
         this->tasks = input_task;
         this->n_tasks = N;
         this->graph = input_graph;
         this->k_cores = K;
         this->local_table = l_table;
         this->send_time = T_s;
         this->cloud_time = T_c;
         this->receive_time = T_r;
         this->power = powers;
         // you can do this as well, since methods are members as well

         // this->prority();
         this->Init_schedule();
         // this->Outerloop();
         this->Migration_algorithm();

    }
    // Step One: Initial Scheduling Algorithm
    std::vector<Matrix> Init_schedule() {
        // Primary assignment
        // std::cout << this->n_tasks << '\n';
        std::vector<int> T_l_min(n_tasks, 0);
        for (int i = 0; i < n_tasks; i++) {
            T_l_min[i] = this->local_table[i][k_cores-1];
        }
        // printMatrix(this->local_table);
        int T_re = send_time + cloud_time + receive_time;
        Matrix T_i = {{},{}};

        // Task prioritizing
        vector<float> w(this->n_tasks,0.0);
        for (int i = 0; i < n_tasks; i++) {
            if (T_re < T_l_min[i]){
                // cloud cumpution T_i = 1;
                T_i[0].push_back(T_re);
                T_i[1].push_back(1);
                w[i] = T_re;
            }else{
                T_i[0].push_back(T_l_min[i]);
                T_i[1].push_back(0);
                w[i] = std::accumulate(this->local_table[i].begin(),this->local_table[i].end(), 0.0)/k_cores;

            }
        }
        vector<float> priorities(this->n_tasks,0);
        for (int i = 0; i < this->n_tasks; i++) {
            priorities[i] = Getpriority(w,i);
        }
        // printVectorf(priorities);
        // Execution unit selection
        std::vector<int> selection = Order(priorities);

        // printVectori(selection);
        Matrix finish_time;
        Matrix ready_time;
        for (int i = 0; i < 4; i++) {
            /* first line is local finish time
            second line is sending finish time
            third line is cloud computing finish time
            forth line is receiving finish time*/
            std::vector<int> tt(n_tasks,0);
            finish_time.push_back(tt);
            if (i <3) {
                ready_time.push_back(tt);
            }
        }
        Matrix schedule;
        for (int i = 0; i < this->k_cores+3; i++) {
            std::vector<int> time_line;
            time_line = {0};
            schedule.push_back(time_line);
        }

        for (int i = 0; i < n_tasks; i++) {
            int task_i = selection[i];
            std::vector<int> pred_i = Getpred(task_i);
            int ready_t_l = 0;
            int ready_t_ws = 0;
            int max_ft_c_j = 0;

            for (int j = 0; j <  pred_i.size(); j++) {
                // RT_l_i = max{max(FT_l_j, FT_wr_j)}
                // ---> max(FT_l_j, FT_wr_j) = FT_l_j + FT_wr_j
                int max_ft_l = finish_time[0][pred_i[j]]+finish_time[3][pred_i[j]];
                ready_t_l = max(max_ft_l,ready_t_l);

                int max_ft_r = finish_time[0][pred_i[j]]+finish_time[1][pred_i[j]];
                ready_t_ws = max(max_ft_r, ready_t_ws );

                max_ft_c_j = max(finish_time[2][pred_i[j]], max_ft_c_j);
            }
            ready_time[0][task_i] = ready_t_l;
            ready_time[1][task_i] = ready_t_ws;
            int ready_t_c = max_ft_c_j;
            if (T_i[1][task_i] == 1) {

                /* task choose cloud computation */
                finish_time[0][task_i] = 0; // local finish time of task i is 0
                finish_time[1][task_i] = max(ready_t_ws, schedule[k_cores][0])+ this->send_time;

                // RT_c_i = max{FT_ws_i, max{FT_c_j}}
                ready_t_c = max(finish_time[1][task_i], max_ft_c_j);

                finish_time[2][task_i] = ready_t_c +this->cloud_time;
                finish_time[3][task_i] = finish_time[2][task_i] + this->receive_time;

                schedule[k_cores][0] = finish_time[1][task_i];
                schedule[k_cores].push_back(task_i);

                schedule[k_cores+1][0] = finish_time[2][task_i];
                schedule[k_cores+1].push_back(task_i);

                schedule[k_cores+2][0] = finish_time[3][task_i];
                schedule[k_cores+2].push_back(task_i);

            }else{
                int temp_core;
                int temp_ft = 9999;
                // check local computation first
                for (int k = 0; k < k_cores; k++) {
                    if (temp_ft > max(schedule[k][0],ready_t_l)+this->local_table[task_i][k]) {
                        temp_ft = max(schedule[k][0],ready_t_l)+this->local_table[task_i][k];
                        temp_core = k;
                    }
                }
                // check if cloud computation finish earier than cloud
                int real_start_t_ws = max(ready_t_ws, schedule[k_cores][0]);

                ready_t_c = max(real_start_t_ws + this->send_time, max_ft_c_j);
                if (temp_ft > ready_t_c + this->cloud_time+ this->receive_time) {
                    temp_ft = ready_t_c + this->cloud_time+this->receive_time;
                    temp_core = this->k_cores+2;
                }

                // std::cout << "for task: "<< task_i+1<<" finish time is "<< temp_ft << " at time_line: "<< temp_core<< '\n';
                if (temp_core < k_cores) {
                    // use local computation
                    finish_time[0][task_i] = temp_ft;
                    for (int ow = 1; ow < 4; ow++) {
                        finish_time[ow][task_i] = 0;
                    }
                    schedule[temp_core][0] = temp_ft;
                    schedule[temp_core].push_back(task_i);
                }else{
                    // use cloud
                    T_i[1][task_i] = 1;
                    finish_time[0][task_i] = 0;
                    finish_time[1][task_i] = real_start_t_ws + this->send_time;
                    finish_time[2][task_i] = ready_t_c  +this->cloud_time;
                    finish_time[3][task_i] = temp_ft;
                    schedule[k_cores][0] = finish_time[1][task_i];
                    schedule[k_cores].push_back(task_i);

                    schedule[k_cores+1][0] = finish_time[2][task_i];
                    schedule[k_cores+1].push_back(task_i);

                    schedule[k_cores+2][0] = finish_time[3][task_i];
                    schedule[k_cores+2].push_back(task_i);

                }
            }
            ready_time[2][task_i] = ready_t_c;

        }
        std::vector<Matrix> result = {ready_time,finish_time, schedule};

        return result;
    }

    std::vector<Matrix> Migration_algorithm(){
        std::vector<Matrix> sche = this->Init_schedule();
        Matrix ready_time = sche[0];
        Matrix finish_time = sche[1];
        Matrix schedule = sche[2];

        // printMatrix(schedule);

        printSchedule(finish_time, schedule);

        int T_total = GetT_total(schedule);
        float E_total = GetE_total_report(schedule);
        std::cout <<'\n'<< "the initail total time is: "<< T_total << '\n';
        float T_max = 1.5*  T_total;

        std::cout << "the initial total energy is: "<< E_total << '\n';
        std::cout << '\n'<< "T_max: "<< T_max << '\n' ;
        Matrix new_ready_time = ready_time;
        Matrix new_schedule = schedule;
        Matrix new_finish_time = finish_time;

        int impro_t = 9999;
        float impro_e = 9999.99;
        float old_e;

        while (true) {
            old_e = impro_e;
            std::vector<Matrix> temp_sche = Outerloop(new_ready_time, new_finish_time, new_schedule, T_max);
            if (temp_sche.empty()) {
                break;
            }
            new_ready_time = temp_sche[0];
            new_finish_time = temp_sche[1];
            new_schedule= temp_sche[2];
            impro_e = GetE_total(new_schedule); // energy
            // std::cout << impro_e << '\n';
            impro_t = GetT_total(new_schedule);
            if (old_e < impro_e) {
                break;
            }
        }
        // std::cout << "================" << '\n';
        float e = GetE_total_report(new_schedule);
        std::cout << '\n'<< "the improved total time is: "<< impro_t << '\n';
        std::cout << "the improved total energy is: "<< impro_e << '\n'<< '\n';
        // printMatrix(new_schedule);
        printSchedule(new_finish_time, new_schedule);
        return {new_ready_time,new_finish_time, new_schedule};

    }

private:
    vector<int> tasks;
    int n_tasks;
    Matrix graph;
    int k_cores;
    Matrix local_table;
    int send_time;
    int cloud_time;
    int receive_time;
    vector<float> power;
    std::vector<Matrix> Outerloop(Matrix ready_time, Matrix finish_time, Matrix schedule, float T_max) {

        int T_total = GetT_total(schedule);
        float E_total = GetE_total(schedule);
        Matrix new_schedule;
        Matrix new_finish_time;
        Matrix new_ready_time;

        // Outer loop
        std::vector<std::vector<Matrix>> choice;
        std::vector<std::vector<Matrix>> no_choice;
        for (int curr_core = 0; curr_core < k_cores; curr_core++) {
            for (int j = 1; j < schedule[curr_core].size(); j++) {
                int v_i = schedule[curr_core][j];
                // std::cout << "========================" << '\n'<< "v_i: "<< v_i<<'\n';

                for (int k = 0; k < k_cores+1; k++) {
                    if(k != curr_core){
                        std::vector<Matrix> temp_sche = kernal(curr_core, j, v_i, k, ready_time, finish_time, schedule);
                        if (GetE_total(temp_sche[2]) < E_total) {
                            if (GetT_total(temp_sche[2]) <= T_total) {
                                choice.push_back(temp_sche);
                            }else{
                                no_choice.push_back(temp_sche);
                                // std::cout << "size of ts: "<< temp_sche.size() << '\n';
                            }
                        }
                    }
                }
            }
        }
        std::vector<Matrix> result_sche;
        if (!choice.empty()) {
            /* find the one with lowest energe */
            float lowest_e = 9999.9;
            int save_i;
            for (int i = 0; i < choice.size(); i++) {

                float temp_e = GetE_total(choice[i][2]);
                // std::cerr << "energe: "<< temp_e << '\n';
                if (lowest_e > temp_e ){
                    lowest_e = temp_e;
                    save_i = i;

                }
            }
            result_sche = choice[save_i];
        }else {
            float ratio = 0;
            int save_i = -1;
            for (int i = 0; i < no_choice.size(); i++) {

                float temp_e = GetE_total(no_choice[i][2]);
                // std::cout << " temp_e "<<temp_e << '\n';
                int temp_t =  GetT_total(no_choice[i][2]);
                // std::cout << " temp_t "<<temp_t << '\n';
                // std::cout << "ratio " << (E_total-temp_e)/(temp_t-T_total)<< '\n';
                if ((ratio < (E_total-temp_e)/(temp_t-T_total)) && ( !(float(temp_t) > T_max) )) {
                    ratio = (E_total-temp_e)/(temp_t-T_total);
                    // std::cout << " ratio changed"<< '\n';

                    save_i = i;
                }
            }
            // std::cout << "no choice!!!!!!!!" << '\n';
            if (save_i!= -1) {
                result_sche = no_choice[save_i];
            }
        }

        return result_sche;
    }
    int GetT_total(Matrix schedule){
        int T_total = 0;
        for (int t_l = 0; t_l < schedule.size(); t_l++) {
            T_total = max(schedule[t_l][0], T_total);
        }
        return T_total;
    }
    float GetE_total(Matrix schedule){
        float E_total = 0.0;
        for (int t_l = 0; t_l < schedule.size(); t_l++) {
            for (int i = 1; i < schedule[t_l].size(); i++) {
                if (t_l < k_cores) {
                    // local energy, current task inter is schedule[t_l][i]
                    E_total += power[t_l]*local_table[schedule[t_l][i]][t_l];
                }else if(t_l == k_cores){
                    // sending energy_i
                    E_total += power[t_l]*send_time;
                }
            }
        }
        return E_total;
    }
    float GetE_total_report(Matrix schedule){
        float E_total = 0.0;

        for (int t_l = 0; t_l < schedule.size(); t_l++) {
            if (t_l < k_cores) {
                std::cout << "In the Core "<< t_l+1 << '\n';
            }else if(t_l == k_cores){
                std::cout << "In the wireless sending :"<< '\n';
            }

            for (int i = 1; i < schedule[t_l].size(); i++) {

                if (t_l < k_cores) {
                    // local energy, current task inter is schedule[t_l][i]
                    E_total += power[t_l]*local_table[schedule[t_l][i]][t_l];
                    std::cout << "        Task "<<schedule[t_l][i]<<": "<<
                    power[t_l]<<"  *  "<<
                    local_table[schedule[t_l][i]][t_l] << " = "<<
                    power[t_l]*local_table[schedule[t_l][i]][t_l]<< '\n';

                }else if(t_l == k_cores){
                    // sending energy_i
                    E_total += power[t_l]*send_time;
                    std::cout << "        Task "<<schedule[t_l][i]<<": "<<
                    power[t_l]<<"  *  "<<send_time << " = "<< power[t_l]*send_time<< '\n';

                }
            }
        }
        return E_total;
    }


    vector<Matrix> kernal(int k_ori, int j, int v_tar, int k_tar,
        Matrix ready_time, Matrix finish_time, Matrix schedule) {
        // j is the iteration of v_tar in original timeline.
        // k_tar = num of cores means wireless sending channel
        Matrix new_schedule = schedule;
        // adjust original core scheduling:
        new_schedule[k_ori].erase(new_schedule[k_ori].begin()+j);
        if (k_ori == k_cores) {
            /* if the original core is ws,
            adjust adjust c and wr cores as well */
            new_schedule[k_ori+1].erase(new_schedule[k_ori+1].begin()+j);
            new_schedule[k_ori+2].erase(new_schedule[k_ori+2].begin()+j);
        }
        for (int i = 0; i < k_cores+3; i++) {
            new_schedule[i][0] = 0;
        }

        int RT_tar;  // target task readly time
        if (k_tar < k_cores) {
            /* if the target core is local not cloud */
            RT_tar = ready_time[0][v_tar];  //first line of ready time table is local RT
        }else if (k_tar == k_cores){
            /* if the target core is cloud not local*/
            RT_tar = ready_time[1][v_tar];//second line of ready time table is ws RT
        }
        // std::cout << "the R_tar for "<< v_tar <<" is "<< RT_tar << '\n';

        // finding the position of v_tar in target core:
        new_schedule[k_tar].push_back(-1);
        int position = 1;
        for (int i = 1; i < schedule[k_tar].size(); i++) {
            int v_i = schedule[k_tar][i];
            int ST_i; // start time for every task in thid core
            if(k_tar < k_cores){
                ST_i = finish_time[0][v_i]-local_table[v_i][k_tar];
            }else{
                ST_i = finish_time[1][v_i]-send_time;
            }
            // std::cout << "for v_i: "<< v_i<< " finish time is "<< finish_time[0][v_i] <<" start time is "<< ST_i << '\n';
            if (ST_i < RT_tar){
                position = i+1;
            }else{
                // v_i push the bebind of the the v_tar
                new_schedule[k_tar][i+1] = v_i;
            }
        }
        new_schedule[k_tar][position] = v_tar;
        if (k_tar == k_cores) {
            /* if the target core is ws,
            adjust adjust c and wr cores as well */
            new_schedule[k_tar+1] = new_schedule[k_tar];
            new_schedule[k_tar+2] = new_schedule[k_tar];
        }

        /* reschedule: adjust the finish_time table
        1) initialize two ready vectors */
        std::vector<int> ready1(n_tasks, -1);
        Matrix ready2 = {ready1, ready1};  // save the k in the ready2

        for (int i = 0; i <n_tasks ; i++) {
            ready1[i] = Getpred(i).size();
        }
        for (int i = 0; i < k_cores+1; i++) {
            for (int j = 1; j < new_schedule[i].size(); j++) {
                int v_i = new_schedule[i][j];
                ready2[0][v_i] = j-1;
                ready2[1][v_i] = i;
            }
        }

        // 2) initialize the stack
        std::stack<int> v_stack;
        update_stack(v_stack, ready1, ready2[0]);

        // 3) rescheduling all tasks
        while (!v_stack.empty()) {
            int v_i = v_stack.top();
            int curr_k = ready2[1][v_i];
            v_stack.pop();

            std::vector<int> pred_i = Getpred(v_i);
            int ready_t_l = 0;
            int ready_t_ws = 0;
            int max_ft_c_j = 0;

            for (int j = 0; j <  pred_i.size(); j++) {
                int max_ft_l = finish_time[0][pred_i[j]]+finish_time[3][pred_i[j]];
                ready_t_l = max(max_ft_l,ready_t_l);

                int max_ft_r = finish_time[0][pred_i[j]]+finish_time[1][pred_i[j]];
                ready_t_ws = max(max_ft_r, ready_t_ws );

                max_ft_c_j = max(finish_time[2][pred_i[j]], max_ft_c_j);

            }
            ready_time[0][v_i] = ready_t_l;
            ready_time[1][v_i] = ready_t_ws;
            int ready_t_c = max_ft_c_j;
            if (curr_k == k_cores) {
                /* if this task is wireless sending in new_schedule */
                finish_time[0][v_i] = 0; // local finish time of task i is 0
                finish_time[1][v_i] = max(ready_t_ws, new_schedule[k_cores][0])+ send_time;

                ready_t_c = max(finish_time[1][v_i], max_ft_c_j);
                ready_time[2][v_i] = ready_t_c;

                finish_time[2][v_i] = ready_t_c+cloud_time;
                finish_time[3][v_i] = finish_time[2][v_i]+receive_time;

                new_schedule[k_cores][0] = finish_time[1][v_i];
                new_schedule[k_cores+1][0] = finish_time[2][v_i];
                new_schedule[k_cores+2][0] = finish_time[3][v_i];

            }else{
                /* local cores */

                finish_time[0][v_i] = max(new_schedule[curr_k][0],ready_t_l)+local_table[v_i][curr_k];
                for (int ow = 1; ow < 4; ow++) {
                    finish_time[ow][v_i] = 0;
                }
                new_schedule[curr_k][0] = finish_time[0][v_i];

            }
            // update ready vectors
            // std::cout << "v_i is: "<< v_i << '\n';

            for (int i = 0; i < k_cores+1; i++) {
                for (int j = 1; j < new_schedule[i].size(); j++) {
                    int v_j = new_schedule[i][j];
                    if (graph[v_i][v_j] ==1) { // v_j is succ of v_i
                        ready1[v_j] -= 1;
                    }
                    if (v_j == v_i) {
                        for (int k = j+1; k < new_schedule[i].size(); k++) {
                            ready2[0][new_schedule[i][k]] -= 1;
                        }
                    }
                }
            }
            update_stack(v_stack, ready1, ready2[0]);
        }

        // Matrix new_finish_time = finish_time;




        return {ready_time, finish_time, new_schedule};
    }
    void update_stack(std::stack<int> &v_stack, std::vector<int> &r1,
        std::vector<int> &r2) {
        for (int i = 0; i < n_tasks; i++) {
            // std::cout << "r1[i] = "<< r1[i] <<"||   r2[i] = "<< r2[i]<< '\n';
            if ((r1[i] ==0) && (r2[i] ==0) ){
                // std::cout <<"for v_i "<< new_schedule[k_tar][i+1]<<" r1 = 0, and r2 = 0" << '\n';
                v_stack.push(i);
                r1[i] = -1;
                r2[i] = -1;
            }
        }

    }
    void PrintStack(stack<int> s){
        if (s.empty()){
            return;
        }
        int x = s.top();
        s.pop();
        PrintStack(s);
        cout << x << " ";
        s.push(x);
    }
    void printSchedule(Matrix finish_time, Matrix schedule) {
        /* code */
        for (int k = 0; k < k_cores; k++) {
            std::cout << "Core "<<k+1<<" :" << '\n';
            for (int i = 1; i < schedule[k].size(); i++) {
                int task = schedule[k][i];
                std::cout << "  Tesk " <<tasks[task]<< " from "<<finish_time[0][task]-local_table[task][k] <<" to "<< finish_time[0][task]<< '\n';
            }
        }
        std::cout << "Sending: " << '\n';
        for (int i = 1; i < schedule[k_cores].size(); i++) {
            int task = schedule[k_cores][i];
            std::cout << "  Tesk "<< tasks[task]<< " from "<<finish_time[1][task]-send_time<<" to "<< finish_time[1][task]<< '\n';
        }
        std::cout << "Cloud : " << '\n';
        for (int i = 1; i < schedule[k_cores+1].size(); i++) {
            int task =schedule[k_cores+1][i];
            std::cout << "  Tesk "<< tasks[task]<< " from "<<finish_time[2][task]-cloud_time<<" to "<< finish_time[2][task]<< '\n';
        }
        std::cout << "Receiving: " << '\n';
        for (int i = 1; i < schedule[k_cores+2].size(); i++) {
            int task = schedule[k_cores+2][i];
            std::cout << "  Tesk "<< tasks[task]<< " from "<<finish_time[3][task]-receive_time<<" to "<< finish_time[3][task]<< '\n';
        }
    }
    void printMatrix(Matrix m) {
        std::cout << "";

        for(int i=0; i < m.size(); i++){
             for (int j = 0; j < m[i].size(); j++) {
                 std::cout << m[i].at(j) << ' ';
             }
             std::cout << '\n';
        }
        std::cout << '\n';
    }
    void printArray(int arr[], int n){
        std::cout << " ";
        for (int i = 0; i < n; i++)
            std::cout << " " << arr[i];
        std::cout << std::endl;
    }
    void printVectori(vector <int> const &a) {
       std::cout << "";

       for(int i=0; i < a.size(); i++){
            std::cout << a.at(i) << ' ';
       }
       std::cout << '\n';
    }
    void printVectorf(vector <float> const &a) {
       std::cout << "";
       for(int i=0; i < a.size(); i++){
            std::cout << a.at(i) << ' ';
       }
       std::cout << '\n';
    }
    float Get_w(int i){
        int T_l_min = local_table[i][2];
        int T_re = send_time + cloud_time + receive_time;
        if (T_re < T_l_min){
            return T_re;
        }else{
            return std::accumulate(local_table[i].begin(),local_table[i].end(), 0.0)/k_cores;

        }
    }
    float Getpriority(vector<float> w, int i){
        if (std::accumulate(graph[i].begin(), graph[i].end(), 0) == 0){
            return w[i];
        }else{
            vector<float> succs_pro;
            for (int j = 0; j < n_tasks; j++) {
                if (graph[i][j] == 1) {
                    succs_pro.push_back(Getpriority(w, j));
                }
            }
            return w[i]+ *max_element(succs_pro.begin(), succs_pro.end());
        }
    }
    std::vector<int> Order(vector<float> priorities){
        int i = 0;
        std::vector<int> selection;
        std::vector< std::pair<int, float> > pairs;
    	for (auto itr = priorities.begin(); itr != priorities.end(); ++itr, i++)
        	pairs.push_back(std::pair<int ,float>(i, *itr));

    	sort(pairs.begin(), pairs.end(), [=](std::pair<int, float>& a, std::pair<int, float>& b)
    	{
    	    return a.second > b.second;
    	}
    	);

    	i = 0;
    	for(auto [task_id, p] : pairs){
    		// std::cout << "order " << i << std::endl;
    		// std::cout << "\ttask " << task_id << ": " << p << std::endl;
            selection.push_back(task_id);
    		i++;
    	}
        return selection;
    }

    std::vector<int> Getpred(int task){
        std::vector<int> pred;
        for (int i = 0; i < n_tasks; i++) {
            if (graph[i][task] == 1){
                pred.push_back(i);
            }
        }
        return pred;
    }

};

void printMatrix(Matrix m) {
    std::cout << "";

    for(int i=0; i < m.size(); i++){
         for (int j = 0; j < m[i].size(); j++) {
             std::cout << m[i].at(j) << ' ';
         }
         std::cout << '\n';
    }
    std::cout << '\n';
}

int main(int argc, char const *argv[]) {
    vector<int> tasks1 = {1,2,3,4,5,6,7,8,9,10};
    Matrix input_graph1 ={{0,1,1,1,1,1,0,0,0,0},
                         {0,0,0,0,0,0,0,1,1,0},
                         {0,0,0,0,0,0,1,0,0,0},
                         {0,0,0,0,0,0,0,1,1,0},
                         {0,0,0,0,0,0,0,0,1,0},
                         {0,0,0,0,0,0,0,1,0,0},
                         {0,0,0,0,0,0,0,0,0,1},
                         {0,0,0,0,0,0,0,0,0,1},
                         {0,0,0,0,0,0,0,0,0,1},
                         {0,0,0,0,0,0,0,0,0,0}};


    Matrix l_table1 = {{9,7,5},
                      {8,6,5},
                      {6,5,4},
                      {7,5,3},
                      {5,4,2},
                      {7,6,4},
                      {8,5,3},
                      {6,4,2},
                      {5,3,2},
                      {7,4,2}};

    vector<float> powers1 = {1,2,4,0.5};
    // Schedule test1 = Schedule(tasks1, 10, input_graph1, 3, l_table1, 3,1,1, powers1);
    Matrix input_graph2 ={{0,1,1,1,0,0,0,0,0,0},
                         {0,0,0,0,1,1,0,0,0,0},
                         {0,0,0,0,0,1,1,0,0,0},
                         {0,0,0,0,0,0,1,1,0,0},
                         {0,0,0,0,0,0,0,0,1,0},
                         {0,0,0,0,0,0,0,0,1,0},
                         {0,0,0,0,0,0,0,0,1,0},
                         {0,0,0,0,0,0,0,0,1,0},
                         {0,0,0,0,0,0,0,0,0,1},
                         {0,0,0,0,0,0,0,0,0,0}};

    // Schedule test2 = Schedule(tasks1, 10, input_graph2, 3, l_table1, 3,1,1, powers1);
    vector<int> tasks3 = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};

    Matrix input_graph3 ={{0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // 3
                             {0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
                             {0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0},
                             {0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0},
                             {0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0},
                             {0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0},
                             {0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0}, // 3
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
                             {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
        };

    Matrix l_table3 = {{9,7,5},
                      {8,6,5},
                      {6,5,4},
                      {7,5,3},
                      {5,4,2},
                      {7,6,4},
                      {8,5,3},
                      {6,4,2},
                      {5,3,2},
                      {7,4,2},
                      {9,7,5},
                      {8,6,5},
                      {6,5,4},
                      {7,5,3},
                      {5,4,2},
                      {7,6,4},
                      {8,5,3},
                      {6,4,2},
                      {5,3,2},
                      {7,4,2}};
    // std::cout << l_table3.size() << '\n';
    Matrix input_graph4 ={{0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // 4
                         {0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}

    };
    Matrix input_graph5 ={{0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, // 5
                         {0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
                         {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}

    };

    // Schedule test3 = Schedule(tasks3, 20, input_graph3, 3, l_table3, 3,1,1, powers1);
    // Schedule test4 = Schedule(tasks3, 20, input_graph4, 3, l_table3, 3,1,1, powers1);
    Schedule test5 = Schedule(tasks3, 20, input_graph5, 3, l_table3, 3,1,1, powers1);


    vector<int> tasks_min = {1,2,3,4};
    Matrix graph_min = {{0,0,1,1},{0,0,0,1},{0,0,0,0},{0,0,0,0}};
    Matrix table_min = {{9,7,5},{8,7,6},{7,6,3},{7,5,3}};

    vector<int> tasks_min1 = {1,2,3,4,5,6,7,8,9,10,11};
    Matrix graph_min1 ={{0,1,1,1,1,1,0,0,0,0,0},
                         {0,0,0,0,0,0,0,1,1,0,0},
                         {0,0,0,0,0,0,1,0,0,0,0},
                         {0,0,0,0,0,0,0,1,1,0,0},
                         {0,0,0,0,0,0,0,0,1,0,0},
                         {0,0,0,0,0,0,0,1,0,0,0},
                         {0,0,0,0,0,0,0,0,0,1,0},
                         {0,0,0,0,0,0,0,0,0,1,0},
                         {0,0,0,0,0,0,0,0,0,1,0},
                         {0,0,0,0,0,0,0,0,0,0,1},
                         {0,0,0,0,0,0,0,0,0,0,0},
                     };


    Matrix table_min1 = {{9,7,5},
                      {8,6,5},
                      {6,5,4},
                      {7,5,3},
                      {5,4,2},
                      {7,6,4},
                      {8,5,3},
                      {6,4,2},
                      {5,3,2},
                      {7,4,2},{5,4,2}};
    // Schedule test_min = Schedule(tasks_min1, 11, graph_min1, 3, table_min1, 3,1,1, powers1);

    // graph_min[0] = table_min[0];
    // std::cout << graph_min[0].size() << '\n';

    return 0;
}
