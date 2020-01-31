#include <algorithm>
#include <cassert>
#include <climits>
#include <cmath>
#include <fstream>
#include <functional>
#include <future>
#include <iostream>
#include <tuple>
#include <vector>

constexpr size_t n_test = 3;

using namespace std;
struct point
{
    point() : id(), x(), y()
    {
    }
    point(int id_, double x_, double y_) : id(id_), x(x_), y(y_)
    {
    }
    int id;
    double x;
    double y;
};

vector<int> get_range(int start, int end)
{
    vector<int> r(end - start);
    for (int i = start; i < end; i++)
    {
        r.at(i - start) = i;
    }
    return r;
}

double sq_dist(point p1, point p2)
{
    return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
}

vector<point> closest_points(vector<point> &points, point pos, size_t n)
{
    auto f = [pos](point p1, point p2) { return sq_dist(p1, pos) > sq_dist(p2, pos); };
    sort(points.begin(), points.end(), f);

    vector<point> r;
    for (int i = points.size() - n; i < points.size() && !points.empty(); i++)
    {
        r.push_back(points.at(i));
        // cout << pos.x << " " << pos.y << " " << points.at(i).x << " " <<
        // points.at(i).y << " " << sq_dist(points.at(i), pos) << endl;
    }
    return r;
}

template <size_t n> array<point, n> closest_points(vector<point> &points, point pos)
{
    auto f = [pos](point p1, point p2) { return sq_dist(p1, pos) > sq_dist(p2, pos); };
    sort(points.begin(), points.end(), f);

    array<point, n> r;
    for (int i = points.size() - n; i < points.size(); i++)
    {
        r.at(i - points.size() + n) = points.at(i);
        // cout << pos.x << " " << pos.y << " " << points.at(i).x << " " <<
        // points.at(i).y << " " << sq_dist(points.at(i), pos) << endl;
    }
    return r;
}

tuple<point, int> get_next_cell_glouton(vector<point> &points, point pos, double time_left)
{
    point candidate = points.at(0);
    double goodness = sq_dist(candidate, pos);
    int cand_id = 0;
    int id = 0;
    for (point p : points)
    {
        double p_goodness = sq_dist(p, pos);
        if (p_goodness <= goodness)
        {
            candidate = p;
            cand_id = id;
            goodness = p_goodness;
        }
        id++;
    }
    return {candidate, cand_id};
}

tuple<point, int> get_next_cell_heur(vector<point> &points, point pos, double time_left)
{
    point candidate = points.at(0);
    double goodness = sq_dist(candidate, pos);
    int cand_id = 0;
    int id = 0;
    for (point p : points)
    {
        double p_goodness = sqrt(sq_dist(p, pos)) + 0.073 * max(fabs(p.x), fabs(p.y));
        if (p_goodness <= goodness)
        {
            candidate = p;
            cand_id = id;
            goodness = p_goodness;
        }
        id++;
    }
    return {candidate, cand_id};
}

bool play(vector<point> &points, double &time_left, point &pos, int id, point to_play)
{
    time_left -= sqrt(sq_dist(pos, to_play));
    if (time_left > 0)
    {
        points.erase(points.begin() + id);
        pos = to_play;
    }
    return time_left > 0;
}
vector<point> gros_glouton(vector<point> &points, double t,
                           const function<tuple<point, int>(vector<point> &, point, double)> &next_cell, point pos,
                           bool is_main);

vector<vector<int>> get_all_permutations(const vector<int> &f)
{
    vector<vector<int>> permutations;

    permutations.push_back(f);
    permutations.push_back(f);
    while (next_permutation(permutations.back().begin(), permutations.back().end()))
    {
        permutations.push_back(permutations.back());
    }
    permutations.pop_back();
    return permutations;
}

double get_sq_cost(const vector<point> &points, point pos)
{
    double c = 0;
    for (point p : points)
    {
        c += sqrt(sq_dist(pos, p));
        pos = p;
    }
    return c;
}

tuple<point, int> get_next_cell_locally_optimised(vector<point> &points, point pos, int time_left)
{
    vector<point> best_points = closest_points(points, pos, n_test);
    vector<int> id;
    int j = 0;
    for (int i = 0; i < best_points.size(); i++)
    {
        id.push_back(i);
    }
    vector<vector<int>> permutations = get_all_permutations(id);
    vector<double> costs(permutations.size());

    auto f = [&](int i) { return best_points.at(i); };

    for (int i = 0; i < permutations.size(); i++)
    {
        vector<point> permp(best_points.size());
        transform(permutations.at(i).begin(), permutations.at(i).end(), permp.begin(), f);
        costs.at(i) = get_sq_cost(permp, pos);
    }

    vector<int> perm_id = get_range(0, permutations.size());
    auto cmp = [&](int perm1, int perm2){return costs.at(perm1) < costs.at(perm2);};

    int best_id = min_element(costs.begin(), costs.end()) - costs.begin();

    point best_point = best_points.at(permutations.at(best_id).at(0));

    return {best_point, points.size() - n_test + permutations.at(best_id).at(0)};
    // auto f = [&goodnesses](int i, int j) { return goodnesses.at(i) < goodnesses.at(j); };

    // int min_id = *max_element(id.begin(), id.end(), f);

    // return {best_points.at(min_id), points.size() - n_test + min_id};
}

tuple<point, int> get_next_cell_mieux(vector<point> &points, point pos, int time_left)
{
    array<point, n_test> best_points = closest_points<n_test>(points, pos);
    array<int, n_test> goodnesses{0};
    array<int, n_test> id{0};
    int j = 0;
    array<future<void>, n_test> futures;
    array<tuple<vector<point>, point, double>, n_test> thread_mem;
    for (int i = 0; i < n_test; i++)
    {

        get<0>(thread_mem.at(i)) = points;
        get<1>(thread_mem.at(i)) = pos;
        get<2>(thread_mem.at(i)) = time_left;
        auto value = [&np = get<0>(thread_mem.at(i)), &n_time_left = get<2>(thread_mem.at(i)),
                      &npos = get<1>(thread_mem.at(i)), &best_points, &id, i, &goodnesses]() {
            if (play(np, n_time_left, npos, np.size() - n_test + i, best_points.at(i)))
            {
                goodnesses.at(i) = gros_glouton(np, n_time_left, function(get_next_cell_glouton), npos, false).size();
            }
            else
            {
                goodnesses.at(i) = -1;
            }
            id.at(i) = i;
        };

        futures.at(i) = async(value);
    }
    for (int i = 0; i < n_test; i++)
    {
        futures.at(i).get();
    }

    auto f = [&goodnesses](int i, int j) { return goodnesses.at(i) < goodnesses.at(j); };

    int min_id = *max_element(id.begin(), id.end(), f);

    return {best_points.at(min_id), points.size() - n_test + min_id};
}


tuple<point, int> get_next_cell_mieux_mieux(vector<point> &points, point pos, int time_left)
{
    array<point, n_test> best_points = closest_points<n_test>(points, pos);
    array<int, n_test> goodnesses{0};
    array<int, n_test> id{0};
    int j = 0;
    array<future<void>, n_test> futures;
    array<tuple<vector<point>, point, double>, n_test> thread_mem;
    for (int i = 0; i < n_test; i++)
    {

        get<0>(thread_mem.at(i)) = points;
        get<1>(thread_mem.at(i)) = pos;
        get<2>(thread_mem.at(i)) = time_left;
        auto value = [&np = get<0>(thread_mem.at(i)), &n_time_left = get<2>(thread_mem.at(i)),
            &npos = get<1>(thread_mem.at(i)), &best_points, &id, i, &goodnesses]() {
            if (play(np, n_time_left, npos, np.size() - n_test + i, best_points.at(i)))
            {
                goodnesses.at(i) = gros_glouton(np, n_time_left, function(get_next_cell_mieux), npos, false).size();
            }
            else
            {
                goodnesses.at(i) = -1;
            }
            id.at(i) = i;
        };

        futures.at(i) = async(value);
    }
    for (int i = 0; i < n_test; i++)
    {
        futures.at(i).get();
    }

    auto f = [&goodnesses](int i, int j) { return goodnesses.at(i) < goodnesses.at(j); };

    int min_id = *max_element(id.begin(), id.end(), f);

    return {best_points.at(min_id), points.size() - n_test + min_id};
}

vector<point> gros_glouton(vector<point> &points, double t,
                           const function<tuple<point, int>(vector<point> &, point, double)> &next_cell, point pos,
                           bool is_main)
{
    double time_left = t;
    vector<point> result;
    while (time_left > 0 && !points.empty())
    {
        auto [point, id] = next_cell(points, pos, time_left);

        if (play(points, time_left, pos, id, point))
        {
            result.push_back(point);
            if (is_main)
            {
                cerr << "cell chosen : " << point.x << " " << point.y << " |  time left : " << time_left
                     << " | number_of_points : " << points.size() << endl;
            }
        }
    }

    return result;
}

void output(const vector<point> &points)
{
    for (point p : points)
    {
        cout << p.id << endl;
    }
}

int main()
{
    ifstream inn;
    inn.open("input_2.txt");

    int t;
    assert(inn >> t);
    vector<point> points;
    int id;
    double x, y;

    while (inn >> id >> x >> y)
    {
        points.emplace_back(id, x, y);
    }
    vector<point> miam = gros_glouton(points, double(t), function(get_next_cell_mieux), point(-1, 0, 0), true);
    output(miam);
    cerr << "number of cells eaten : " << miam.size() << endl;
    return 0;
}
