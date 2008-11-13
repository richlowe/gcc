// PR middle-end/17411
// { dg-do compile }
// { dg-options "" }

struct CalibData {
  float mean_pedestal;
};

struct pair
{
  CalibData second;
  pair(const CalibData& __b) : second(__b) { }
};

void insert(const pair& __x);

void foo()
{
  insert(pair(CalibData()));
}

